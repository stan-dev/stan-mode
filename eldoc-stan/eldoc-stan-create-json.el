;;; eldoc-stan-create-json.el --- Create JSON file for eldoc-stan  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kazuki Yoshida

;; Author: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; URL: https://github.com/stan-dev/stan-mode/tree/master/eldoc-stan
;; Keywords: help, tools
;; Version: 10.3.0
;; Created: 2019-07-29
;; Package-Requires: ((emacs "25.1") (stan-mode "10.3.0"))

;; This file is not part of GNU Emacs.

;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Users should not have to deal with this file.
;;
;; The following make commend will generate the JSON data file required
;; by eldoc-stan.el.
;;
;; $ make build-src

;;; Code:
;; For thread-last
(require 'subr-x)
;; For json-read-file and json-encode
(require 'json)

;;; The following functions are for lookup data generation.
(defun eldoc-stan-create-json--string-join-with-comma (drop-first lst)
  "`string-join' with special handling of first element.

Joins sequence of strings using , as a separator.
When DROP-FIRST is t, the first element is dropped.
When DROP-FIRST is symbol `given', the separator is |
between the first and second elements.
LST is a list of strings."
  ;; https://stackoverflow.com/questions/12999530/is-there-a-function-that-joins-a-string-into-a-delimited-string
  ;; drop-first vector element if drop-first
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequence-Functions.html
  (thread-first (cond
                 ;; If using | and two or more aguments exist.
                 ((and (eq drop-first 'given)
                       (>= (length lst) 2))
                  (cons (concat (nth 0 lst)
                                " | "
                                (nth 1 lst))
                        (seq-drop lst 2)))
                 ;; Drop first if asked
                 ((eq drop-first t) (seq-drop lst 1))
                 ;; Otherwise do not mess with it.
                 (t lst))
    (string-join ", ")))

(defun eldoc-stan-create-json--signature-to-string (signature drop-first)
  "Extract function type and name from a signature hash table.

Expects SIGNATURE to contain `args' entry, which is a vector.
The vector should contain a series of `type' and `name' pairs.

DROP-FIRST is directly passed to `eldoc-stan-create-json--string-join-with-comma'."
  ;;
  (thread-last signature
    (gethash "args")
    (mapcar (lambda (el)
              (concat (gethash "type" el)
                      " "
                      (gethash "name" el))))
    ;; Here we have list of "type name" strings.
    (eldoc-stan-create-json--string-join-with-comma drop-first)))

(defun eldoc-stan-create-json--signatures-to-list-of-strings (signatures drop-first)
  "Extract function type and name from a list of signature hash tables.

SIGNATURES is a list of signature hash tables.
DROP-FIRST is passed to `eldoc-stan-create-json--signature-to-string'."
  ;;
  (mapcar (lambda (signature)
            (eldoc-stan-create-json--signature-to-string
             signature
             drop-first))
          signatures))

(defun eldoc-stan-create-json--density-function? (fun-ht)
  "Check density function status given a function hash table.

FUN-HT is a hash table corresponding to a single function."
  (let ((slot (gethash "density" fun-ht)))
    (cond ((equal slot json-false)
           nil)
          ((equal slot json-null)
           (error "The density slot should not have `json-null'!"))
          (t t))))

(defun eldoc-stan-create-json--lcdf-function? (fun-ht)
  "Check lcdf function status given a function hash table.

FUN-HT is a hash table corresponding to a single function."
  (let ((slot (gethash "lcdf" fun-ht)))
    (cond ((equal slot json-false)
           nil)
          ((equal slot json-null)
           (error "The lcdf slot should not have `json-null'!"))
          (t t))))

(defun eldoc-stan-create-json--lccdf-function? (fun-ht)
  "Check lccdf function status given a function hash table.

FUN-HT is a hash table corresponding to a single function."
  (let ((slot (gethash "lccdf" fun-ht)))
    (cond ((equal slot json-false)
           nil)
          ((equal slot json-null)
           (error "The lccdf slot should not have `json-null'!"))
          (t t))))

(defun eldoc-stan-create-json--extract-sampling-function (fun-ht)
  "Extract the sampling function given a function hash table.

FUN-HT is a hash table corresponding to a single function."
  (gethash "sampling" fun-ht))


(defun eldoc-stan-create-json--create-function-hash-table (stan-lang-json-path given)
  "Create a hash table for function argument look up from JSON.

The JSON file to be specified via STAN-LANG-JSON-PATH is the
full definitions file in the `stan-language-definitions' repo.
The GIVEN option decides whether to use | notation for density
type functions (i.e., *_lpdf and *_lpmf)."
  ;;
  (let* ((json-functions
          (let* ((json-object-type 'hash-table)
                 (json-array-type 'list)
                 (json-key-type 'string)
                 (json (json-read-file stan-lang-json-path)))
            ;;
            (thread-last json
              (gethash "functions"))))
         ;; Extract function names as a list.
         (lst-fun-names (hash-table-keys json-functions))
         ;; Create an empty hash table to hold a list of eldoc
         ;; strings for each function name.
         (eldoc-stan-create-json--args-hash-table (make-hash-table :test 'equal)))
    ;;
    ;; Invoke mapc for side effects on `eldoc-stan-create-json--args-hash-table'.
    ;; http://ergoemacs.org/emacs/elisp_hash_table.html
    ;; Map over function names for side effects
    (mapc
     (lambda (fun-name)
       ;; Extract sub-hash table for a specific function
       (let* ((fun-ht (gethash fun-name json-functions))
              (signatures (gethash "signatures" fun-ht)))
         (cond
          ;; Handle density type function in a special way.
          ((eldoc-stan-create-json--density-function? fun-ht)
           ;; This is for a *_lpdf or *_lpmf.
           (puthash fun-name
                    (eldoc-stan-create-json--signatures-to-list-of-strings
                     signatures
                     ;; Special handling of the first argument
                     ;; via `drop-first' argument. Note only
                     ;; these two options make sense here.
                     ;;  'given: Use |
                     ;;  nil: Do not drop
                     (if given 'given nil))
                    eldoc-stan-create-json--args-hash-table)
           ;; Also add the sampling function.
           (puthash (eldoc-stan-create-json--extract-sampling-function
                     fun-ht)
                    (eldoc-stan-create-json--signatures-to-list-of-strings
                     signatures
                     ;; Drop first argument as it is on LHS of ~.
                     t)
                    eldoc-stan-create-json--args-hash-table))
          ;; *_lcdf and *_lccdf also needs special handling
          ((or (eldoc-stan-create-json--lcdf-function? fun-ht)
               (eldoc-stan-create-json--lccdf-function? fun-ht))
           ;; Same behavior as the first part for density.
           (puthash fun-name
                    (eldoc-stan-create-json--signatures-to-list-of-strings
                     signatures
                     (if given
                         'given
                       nil))
                    eldoc-stan-create-json--args-hash-table))
          ;;
          ;; Functions other than *_lpdf and *_lpmf.
          (t (puthash fun-name
                      (eldoc-stan-create-json--signatures-to-list-of-strings
                       signatures nil)
                      eldoc-stan-create-json--args-hash-table)))))
     ;; Function names to mapc over..
     lst-fun-names)
    ;; Return the completed arguments hash table.
    eldoc-stan-create-json--args-hash-table))


(defun eldoc-stan-create-json--write-args-json (eldoc-stan-create-json--args-hash-table
                                                eldoc-stan-json-path)
  "Write ELDOC-STAN-CREATE-JSON--ARGS-HASH-TABLE to a JSON file.

Specify the path with ELDOC-STAN-JSON-PATH.
This JSON file is to be loaded by the library."
  (let* ((json-encoding-pretty-print t)
         (json-encoding-lisp-style-closings t))
    ;;
    (write-region (json-encode eldoc-stan-create-json--args-hash-table)
                  nil
                  eldoc-stan-json-path)))

(provide 'eldoc-stan-create-json)
;;; eldoc-stan-create-json.el ends here
