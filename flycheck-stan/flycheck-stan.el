;;; flycheck-stan.el --- Add Stan support for Flycheck -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Jeffrey Arnold,
;;               2019 Kazuki Yoshida

;; Author: Jeffrey Arnold <jeffrey.arnold@gmail.com>,
;;         Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; URL: https://github.com/stan-dev/stan-mode/tree/master/flycheck-stan
;; Keywords: c,languages
;; Version: 10.0.0
;; Created: 2014-12-19
;; Package-Requires: ((emacs "25.1") (flycheck "0.16.0") (stan-mode "10.0.0"))

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
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>

;;; Commentary:
;;
;; Add a Stan syntax checker to Flycheck which uses stanc.
;;

;;; Code:
(require 'stan-mode)
(require 'rx)
(require 'flycheck)
;; For thread-last
(require 'subr-x)

;;; References:
;; Adding a syntax checker to Flycheck
;;  https://www.flycheck.org/en/latest/developer/developing.html


;;;
;;; Customizable variables
(defgroup flycheck-stan nil
  "Flycheck using stanc."
  :tag "Flycheck Stan"
  :prefix "flycheck-stan-"
  :group 'languages)


;;;
;;; Define helper functions
(defun flycheck-stan-enhance-rx-buffer-locally ()
  "Enhances `rx' buffer locally with `flycheck' elements.

`flycheck' adds keywords `line', `column', `file-name',
`message', and `id' to `rx-constituents' defined in `rx.el'
to handle error message parsing.

The body was taken from `flycheck-rx-to-string'.

This function is intended for use in the `re-builder'
to enhance the pattern for interactive `rx' building."
  (interactive)
  (setq-local rx-constituents
              (append
               `((line . ,(rx (group-n 2 (one-or-more digit))))
                 (column . ,(rx (group-n 3 (one-or-more digit))))
                 (file-name flycheck-rx-file-name 0 nil)
                 (message flycheck-rx-message 0 nil)
                 (id flycheck-rx-id 0 nil))
               rx-constituents nil)))


;;;
;;; Define a parser
;;;  Regexp definitions
;;
;; flycheck uses extended `rx' to describe regexp.
;; `rx.el' is a built-in Emacs module for declarative regular
;; expressions. Look for the documentation of the `rx' function
;; inside Emacs for its usage. Flycheck extends `rx' with a few
;; constructs listed below. (taken from help for ).
;; These were taken from the docstring for `flycheck-rx-to-string',
;; which is used to convert these into string regexps.
;;
;; `line'
;;      matches the line number.
;; `column'
;;      matches the column number.
;; `(file-name SEXP ...)'
;;      matches the file name.  SEXP describes the file name.  If no
;;      SEXP is given, use a default body of `(minimal-match
;;      (one-or-more not-newline))'.
;; `(message SEXP ...)'
;;      matches the message.  SEXP constitutes the body of the
;;      message.  If no SEXP is given, use a default body
;;      of `(one-or-more not-newline)'.
;; `(id SEXP ...)'
;;      matches an error ID.  SEXP describes the ID.
;;
;; These are used in `flycheck-stan-parser' rather than in
;; :error-patterns.  A more conventional approach using regexps
;; in :error-patterns was not successful because the filename
;; part of all patterns overlap at the header.  This resulted
;; in only the first match being recognized due to the design
;; of `flycheck-tokenize-output-with-patterns'.
;;
(defvar flycheck-stan--rx-input-file
  '(seq line-start "Input file=" (file-name) "\n")
  "An `rx' regexp for the input file name.")

(defvar flycheck-stan--rx-error-with-line-column
  '(seq
    (message
     "error in " "'" (file-name) "'"
     " at line " line ", column " column "\n"
     (one-or-more (or not-newline "\n" "\r"))
     ;; To avoid trailing empty lines.
     (not (any whitespace "\n" "\r"))))
  "An `rx' regexp for `stanc' error with `line' and `column' information.
Note that the file name is captured from the message.")

(defvar flycheck-stan--rx-error-with-line-only
  '(seq
    (message
     "PARSER FAILED TO PARSE INPUT COMPLETELY"
     (one-or-more (or not-newline "\n" "\r"))
     "STOPPED AT LINE " line ":"
     (one-or-more (or not-newline "\n" "\r"))
     ;; To avoid trailing empty lines.
     (not (any whitespace "\n" "\r"))))
  "An `rx' regexp for parser failure with `line' only.")

(defvar flycheck-stan--rx-error-no-include-file
  '(seq
    (message
     "could not find include file"
     (one-or-more (or not-newline "\n" "\r"))
     ;; To avoid trailing empty lines.
     (not (any whitespace "\n" "\r"))))
  "An `rx' regexp for parser failure with #include issues.")

;;;  Define fucntion for loading error message starters
(defun flycheck-stan--list-of-strings-from-file (file)
  "Convert FILE into a list of string corresponding to each line."
  ;; http://ergoemacs.org/emacs/elisp_read_file_content.html
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t)))

(defvar flycheck-stan-regexp-error-msgs-start
  (thread-last (flycheck-stan--list-of-strings-from-file
                ;; Convert filename NAME to absolute, and canonicalize it.
                (expand-file-name
                 "./error_msgs.txt"
                 ;; Return the directory component in file name FILENAME.
                 (file-name-directory
                  ;; Full name of file being loaded by load.
                  (or load-file-name
                      buffer-file-name))))
    ;; Optimize as one regexp
    (regexp-opt)
    ;; Need ^ to make sure they are at the beginning.
    (concat (rx line-start)))
  "Regular expression for the beginning of error messages.

This consists of error messege starters in the `error_msgs.txt'
file, which were extracted from:
 stan/src/stan/lang/grammars/semantic_actions_def.cpp
using `flycheck-stan-error-msgs'.

Some error messages were not covered this way.  As such, their
beginnings are included in an ad-hoc manner.

This regexp is used to add Error: in front of these messages to
make the message consistent.

All the info messages are assumed to start with Info:, so they
are not included here.")

;;;  Cleaner definition
(defun flycheck-stan-cleaner (output &optional do-not-add-error)
  "Clean `stan' OUTPUT before parsing and return it.

This should make the parsing process easier.
Remove trailing whitespace.
Remove trailing empty lines at the end of the OUTPUT.
Add Error: to the beginning of know error messages unless given DO-NOT-ADD-ERROR."
  ;; https://www.masteringemacs.org/article/removing-blank-lines-buffer
  ;;
  (with-temp-buffer
    (let ((delete-trailing-lines t))
      (insert output)
      (widen)
      ;; Remove trailing whitespace and trailing empty lines.
      (delete-trailing-whitespace)
      ;; Remove empty lines.
      (flush-lines (rx line-start line-end)
                   (point-min)
                   (point-max))
      ;; Add Error: to known patterns.
      (unless do-not-add-error
        (goto-char (point-min))
        (while (re-search-forward flycheck-stan-regexp-error-msgs-start
                                  nil t)
          ;; \& in NEWTEXT means substitute original matched text.
          ;; FIXEDCASE t to avoid replacing with ERROR: in an all-capital line.
          (replace-match "Error: \\&" t))
        ;; If error in 'file' at ... exists WITHOUT preceding Error:, add it.
        ;; This is for a pattern like the following:
        ;;
        ;; Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments.
        ;;  error in 'examples/example_error_and_info_composite.stan' at line 9, column 2
        ;;   -------------------------------------------------
        ;;      7: }
        ;;      8: parameters {
        ;;      9:   rear mu;
        ;;          ^
        ;;     10:   // The parser stops at the above line.
        ;;   -------------------------------------------------
        (and (goto-char (point-min))
             (re-search-forward (flycheck-rx-to-string
                                 '(seq line-start " error in '" ))
                                nil t)
             (not (re-search-backward (flycheck-rx-to-string
                                       '(seq line-start "Error: " ))
                                      nil t))
             (goto-char (point-min))
             (re-search-forward (flycheck-rx-to-string
                                 '(seq line-start " error in '" ))
                                nil t)
             (replace-match "Error:\n\\&" t)))
      ;; Drop trailing newline at the end of the string
      (while (re-search-forward (rx "\n" buffer-end)
                                nil t)
        (replace-match "" t))
      ;; Return the entire buffer as a plain string.
      (widen)
      (buffer-substring-no-properties (point-min)
                                      (point-max)))))

;;;  Splitter definition
(defun flycheck-stan-splitter (output &optional stanc3)
  "Split OUTPUT into a list of strings.

When STANC3 is nil, splitting happens at Info and Error.
The first list element should the header.
The remaining elements are either an Info or Error.

When STANC3 is non-nil, splitting happens at
- Warning:
- Semantic error in
- Syntax error in
- This should not happen.
All elements are either one of them."
  ;;
  ;; https://discourse.mc-stan.org/t/structured-error-output-format-for-stanc/10342/11?u=kaz-yos
  (let ((split-regexp (if stanc3
                          ;; stanc3 splitting patterns.
                          (rx line-start (or "Warning:"
                                             "Semantic error in"
                                             "Syntax error in"
                                             "This should not happen."))
                        ;; stanc2 splitting patterns.
                        (rx line-start (or "Info"
                                           "Error")))))
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (re-search-forward split-regexp nil t)
        ;; Replacement should not happen if its the very first line.
        (when (> (count-lines 1 (point)) 1)
          ;; \& in NEWTEXT means substitute original matched text.
          (replace-match "\n\\&" t)))
      ;; Split at double newline
      (split-string
       (buffer-substring-no-properties (point-min) (point-max))
       "\n\n" t))))

;;;  Converter definition
(defun flycheck-stan-convert-message-to-error (message buffer checker input-file)
  "Convert a message prefixed with Error or Info into `flycheck-error'.

It expect MESSAGE to contain a single Error or Info message.
The level is determined by the very first line in the message,
which should be either Error: or Info:.  Otherwise, level is nil.

The arguments BUFFER and CHECKER are directly passed to `flycheck-error-new'.
The INPUT-FILE will be used as the file name unless the message itself
contains this information.

Lines cannot be nil.  They are set to 0.
`flycheck-fill-empty-line-numbers' could also be used instead."
  (cond
   ;; Info:
   ((string-match (rx (seq string-start "Info:"))
                  message)
    (flycheck-error-new :buffer buffer
                        :checker checker
                        :filename input-file
                        :line 0
                        :column nil
                        :message message
                        :level 'info
                        :id nil
                        :group nil))
   ;; Error:
   ((string-match (rx (seq string-start "Error:"))
                  message)
    (cond
     ;; Error: with file, line, and column
     ((string-match (flycheck-rx-to-string flycheck-stan--rx-error-with-line-column)
                    message)
      (flycheck-error-new :buffer buffer
                          :checker checker
                          :filename (match-string 1 message)
                          :line (if-let ((line (match-string 2 message)))
                                    (string-to-number line)
                                  0)
                          :column (when-let ((column (match-string 3 message)))
                                    (string-to-number column))
                          :message message
                          :level 'error
                          :id nil
                          :group nil))
     ;; Error: with line
     ((string-match (flycheck-rx-to-string flycheck-stan--rx-error-with-line-only)
                    message)
      (flycheck-error-new :buffer buffer
                          :checker checker
                          :filename input-file
                          :line (if-let ((line (match-string 2 message)))
                                    (string-to-number line)
                                  0)
                          :column nil
                          :message message
                          :level 'error
                          :id nil
                          :group nil))
     ;; Error: without additional info
     ((string-match (flycheck-rx-to-string flycheck-stan--rx-error-no-include-file)
                    message)
      (flycheck-error-new :buffer buffer
                          :checker checker
                          :filename input-file
                          :line 0
                          :column nil
                          :message message
                          :level 'error
                          :id nil
                          :group nil))
     ;; Error: not matching any of the above.
     ;; This is marked with :group 'other_error.
     ;; If this :group is seen, assess whether the pattern matching
     ;; process can be improved to exract more information.
     ;; If an error message matches any one of the patterns defined
     ;; above, :group remains nil.  This decision was made to avoid
     ;; having to test implementations details too much.  If including
     ;; :group for these, each test must specify which regexp pattern
     ;; is expected to be used, which was found to be tedious.
     (t (flycheck-error-new :buffer buffer
                            :checker checker
                            :filename input-file
                            :line 0
                            :column nil
                            :message message
                            :level 'error
                            :id nil
                            :group 'other_error))))
   ;; Remainig ones are unexpected.
   (t (flycheck-error-new :buffer buffer
                          :checker checker
                          :filename input-file
                          :line 0
                          :column nil
                          :message message
                          :level 'error
                          :id nil
                          :group 'unexpected))))

;;;  Parser definition
(defun flycheck-stan-parser (output checker buffer)
  "Parse `stanc' OUTPUT into a list of `flycheck-error' objects.

CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

CHECKER can only be `stanc`.

The BUFFER object is only used to extract the associated stan file name.
This buffer-associated file name is only used when the error message does
not contain a valid header with file name information.

References:
`flycheck-parse-cppcheck' in `flycheck.el'"
  ;;
  (unless (eq 'stanc checker)
    (error "This parser should not be called on an output from a checker other than `stanc'"))
  ;;
  ;; `flycheck-parse-hy-traceback' by lunaryorn.
  ;; https://emacs.stackexchange.com/questions/3755/regexp-to-parse-hy-errors-for-flycheck
  ;;
  ;; ALGORITHM
  ;; Clean stanc output.
  ;;  Remove trailing spaces.
  ;;  Remove empty lines.
  ;;  Add Error: to known error line starters.
  ;; Split cleaned output into the header and individual messages.
  ;;  Insert newlines before know starter keys (i.e., Info: and Error:)
  ;;  Split into list of strings.
  ;; Convert to a list of flycheck-error objects.
  ;;  Extract the header for input file information.
  ;;  Iterate over the remaining list of messages to convert each.
  ;;  Handle Info: and Error: differently.
  ;;   Extract file name if available.
  ;;   Extract line and column information if available.
  ;;
  ;; `flycheck-parse-with-patterns' is the regexp-only equivalent.
  ;; But it avoids duplicated matches, which is problematics for our purpose.
  ;;
  (let* (;; Clean output into a list of string
         (list-output (thread-last output
                        (flycheck-stan-cleaner)
                        (flycheck-stan-splitter)))
         ;; The first list element is usually the header containing
         ;; the input file name, but not always.
         (header (car list-output))
         ;; Extract the input file name from the header.
         ;; This can be nil, if the first element is not the header.
         (input-file (progn
                       (string-match (flycheck-rx-to-string
                                      flycheck-stan--rx-input-file)
                                     header)
                       ;; After searching a string, it must be specified again.
                       ;; Otherwise, it assumes the current buffer.
                       (match-string 1 header)))
         ;; List of Error and Info elements depending on if the header is valid.
         (list-errors (if input-file
                          ;; If the header is valid, use the second element on.
                          (cdr list-output)
                        ;; If not, the first element is also an error message.
                        list-output))
         ;; File name of the BUFFER. Use this if the header is invalid.
         (buff-file-name (buffer-file-name buffer)))
    ;;
    ;; Iterate over the the list of Error and Info.
    (seq-map (lambda (message)
               (flycheck-stan-convert-message-to-error
                message buffer checker
                ;; Use input-file if available.
                ;; If not, use the file name of the BUFFER.
                (or input-file
                    buff-file-name)))
             list-errors)))


;;;
;;; Define a checker
;;
;; flycheck website: Writing a checker.
;;  https://www.flycheck.org/en/latest/developer/developing.html#writing-the-checker
;; flycheck online manual (maybe old): 5 Syntax checker definitions
;;  https://www.flycheck.org/en/27/_downloads/flycheck.html#Syntax-checker-definitions
;;
(flycheck-define-checker stanc
  "A Stan syntax checker using stanc in cmdstan.

References:
 https://mc-stan.org/rstan/reference/stanc.html
 https://mc-stan.org/misc/warnings.html"
  ;;
  ;; The :command specifies the command Flycheck should run to check the buffer.
  ;; It’s a simple list containing the executable and its arguments.
  ;; https://www.flycheck.org/en/27/_downloads/flycheck.html#Defining-syntax-checkers
  :command ("stanc2"
            "--include_paths=."
            source)
  ;; Function to parse an stanc output into the flycheck-error format.
  :error-parser flycheck-stan-parser
  ;; Emacs major modes in which this checker can run
  :modes stan-mode)


;;;###autoload
(defun flycheck-stan-setup ()
  "Set up `flycheck' with `flycheck-stan' checker."
  (add-to-list 'flycheck-checkers 'stanc)
  (flycheck-mode +1))


(provide 'flycheck-stan)

;;;
;;; flycheck-stan.el ends here
