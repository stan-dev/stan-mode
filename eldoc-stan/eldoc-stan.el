;;; eldoc-stan.el --- Eldoc support for stan functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kazuki Yoshida

;; Author: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; URL: http://github.com/stan-dev/stan-mode/eldoc-stan
;; Keywords: help, tools
;; Version: 0.1.0
;; Created: 2019-07-14
;; Package-Requires: ((emacs "25") (stan-mode "9.2.0"))

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
;; This file defines a hash table for looking up stan function eldoc
;; strings and stan eldoc-documentation-function.
;;
;; Usage
;; The `eldoc-documentation-function' function needs to be set to
;; `eldoc-stan-eldoc-documentation-function' buffer locally in
;; `stan-mode'.
;;
;; A convenience function `eldoc-stan-setup' can be added to the
;; mode-specific hook `stan-mode-hook'.
;; (add-hook 'stan-mode-hook #'eldoc-stan-setup)
;;
;; With the `use-package', the following can be used:
;; (use-package eldoc-stan
;;   :hook (stan-mode . eldoc-stan-setup))
;;
;; If you already have a custom stan-mode setup function, you can add
;; the following to its body.
;; (setq-local eldoc-documentation-function
;;             #'eldoc-stan-eldoc-documentation-function)
;;
;; References
;; eldoc-documentation-function
;;  http://doc.endlessparentheses.com/Var/eldoc-documentation-function.html
;; Deep diving into a major mode - Part 2 (IDE Features)
;;  http://www.modernemacs.com/post/major-mode-part-2/
;; Displaying information for string under point
;;  https://emacs.stackexchange.com/questions/18581/displaying-information-for-string-under-point
;; c-eldoc.el: eldoc-mode plugin for C source code
;;  https://github.com/nflath/c-eldoc (available on melpa)
;; c-eldoc.el: Helpful description of the arguments to C/C++ functions and macros
;;  https://github.com/mooz/c-eldoc (uses deferred.el)
;; css-eldoc.el: eldoc-mode plugin for CSS
;;  https://github.com/zenozeng/css-eldoc

;;; Code:
;; For json-read-file and json-encode
(require 'json)
;; For c-eldoc-format-arguments-string
(require 'c-eldoc)
;; defines: c-literal-limits, c-literal-type, c-forward-token-2,
;;          c-backward-sws
(require 'cc-engine)

;;; The following functions are for production use.
;;; Data structure to lookup function signatures.
(defvar eldoc-stan--hash-table
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    ;;
    (json-read-file
     ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Multi_002dfile-Packages.html
     ;; https://emacs.stackexchange.com/questions/30356/how-can-i-find-the-path-of-files-in-an-melpa-package
     ;; Convert filename NAME to absolute, and canonicalize it.
     (expand-file-name
      "./eldoc-stan.json"
      ;; Return the directory component in file name FILENAME.
      (file-name-directory
       ;; Full name of file being loaded by load.
       (or load-file-name
           buffer-file-name)))))
  "Hash table holding a eldoc string for each function name.")


;; Taken and modified from:
;; https://github.com/nflath/c-eldoc/blob/master/c-eldoc.el
(defun eldoc-stan--function-and-argument (&optional limit)
  "Find the current function and position in argument list.

LIMIT sets the limit for the movement and defaults to the point limit.
Minor modification of `c-eldoc-function-and-argument' in
https://github.com/nflath/c-eldoc/blob/master/c-eldoc.el"
  (let* ((literal-limits (c-literal-limits))
         (literal-type (c-literal-type literal-limits)))
    (save-excursion
      ;; if this is a string, move out to function domain
      (when (eq literal-type 'string)
        (goto-char (car literal-limits))
        (setq literal-type nil))
      (if literal-type
          nil
        (c-save-buffer-state ((argument-index 1))
          (while (or (eq (c-forward-token-2 -1 t limit) 0)
                     (when (eq (char-before) ?\[)
                       (backward-char)
                       t))
            (when (or (eq (char-after) ?,)
                      ;; To cover *_lpdf and *_lpmf
                      (eq (char-after) ?|))
              (setq argument-index (1+ argument-index))))
          (c-backward-syntactic-ws)
          (when (eq (char-before) ?\()
            (backward-char)
            (c-forward-token-2 -1)
            (when (looking-at "[a-zA-Z_][a-zA-Z_0-9]*")
              (cons (buffer-substring-no-properties
                     (match-beginning 0) (match-end 0))
                    argument-index))))))))

(defun eldoc-stan--substring-propertize (string start end &rest properties)
  "Return a string with a substring propertized.

STRING is to be manipulated.
START is the zero-based position where properties start (inclusive).
END is the zero-based position where properties ends (exclusive)

The remaining arguments are collected as a list PROPERTIES."
  (let ((before (substring string 0 start))
        (substr (substring string start end))
        (after  (substring string end)))
    (concat before
            (apply #'propertize (cons substr properties))
            after)))

;; Taken and modified from:
;; https://github.com/nflath/c-eldoc/blob/master/c-eldoc.el
(defun eldoc-stan--format-arguments-string (arguments index)
  "Formats the argument list of a function.

ARGUMENTS is a string of arguments.  The expected separators are
vertical | and comma ,.
INDEX is the index of the argument to be highlighted.
It detects the relevant argument location skipping leading
ones using regexp.  See the corresponding test file for
the construction of these regexp."
  (let ((paren-pos (string-match "(" arguments))
        (pos 0))
    ;; This proceeds when `(' exists in the arguments string.
    (when paren-pos
      (setq arguments (replace-regexp-in-string "\\\\?[[:space:]\\\n]"
                                                " "
                                                (substring arguments paren-pos))
            arguments (replace-regexp-in-string "\\s-+" " " arguments)
            arguments (replace-regexp-in-string " *, *" ", " arguments)
            arguments (replace-regexp-in-string "( +" "(" arguments)
            arguments (replace-regexp-in-string " +)" ")" arguments))
      ;; find the correct argument to highlight, taking `...'
      ;; arguments into account
      (while (and (> index 1)
                  pos
                  (not (string= (substring arguments (+ pos 2) (+ pos 6))
                                "...)")))
        ;; Move pos to the next separator and decrease index by 1.
        (setq pos (string-match "[,|]" arguments (1+ pos))
              index (1- index)))
      ;; while loop exits when index == 1.
      ;; pos should be at , right before the current argument.
      ;; embolden the current argument
      (when (and pos
                 (setq pos (string-match "[^ ,|()]" arguments pos)))
        (add-text-properties pos
                             (string-match "[,)]\\| |" arguments pos)
                             '(face eldoc-highlight-function-argument)
                             arguments))
      arguments)))


;;;###autoload
(defun eldoc-stan-eldoc-documentation-function ()
  "Return an eldoc string for the function at point.

Set this as `eldoc-documentation-function' in the `stan-mode'."
  ;;
  (let ((fun-arg (eldoc-stan--function-and-argument)))
    ;; Only proceed if it is non-nil! ("fun_name" . arg-pos) pair.
    (when fun-arg
      ;; Obtain list of signature strings. There can be several.
      (let ((signatures (gethash (car fun-arg) eldoc-stan--hash-table)))
        (when signatures
          ;; Concatenate as a single string. One signature per line.
          (mapconcat (lambda (signature)
                       ;; from c-eldoc.el
                       (eldoc-stan--format-arguments-string
                        ;; Surround with () as required by the function.
                        (concat "( " signature " )")
                        (cdr fun-arg)))
                     ;; SEQUENCE. Vector of strings.
                     signatures
                     ;; SEPARATOR
                     "\n"))))))


;;;###autoload
(defun eldoc-stan-setup ()
  "Set up `eldoc-stan-eldoc-documentation-function'.

Specify `eldoc-stan-eldoc-documentation-function' as
`eldoc-documentation-function'.
Add this function to the `stan-mode-hook'."
  (setq-local eldoc-documentation-function
              #'eldoc-stan-eldoc-documentation-function)
  (eldoc-mode 1))


(provide 'eldoc-stan)
;;; eldoc-stan.el ends here
