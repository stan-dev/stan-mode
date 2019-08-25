;;; flycheck-stan-error-msgs.el --- Add Stan support for Flycheck -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Jeffrey Arnold

;; Author: Jeffrey Arnold <jeffrey.arnold@gmail.com>,
;;         Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; URL: http://github.com/stan-dev/flycheck-stan-error-msgs
;; Keywords: c,languages
;; Version: 1.0.0
;; Created: 2014-12-19
;; Package-Requires: ((emacs "25") (flycheck "0.16.0") (stan-mode "3.0.0"))

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
;; <http://www.gnu.org/licenses/>

;;; Commentary:
;;
;; Use to extract error message starting patterns from
;; /stan/src/stan/lang/grammars/semantic_actions_def.cpp

;;; Code:
(require 'rx)
;; For thread-last
(require 'subr-x)


(defun flycheck-stan-error-msgs--delete-endl (code)
  "Remove endl from the start of error_msgs cpp stream.

CODE is the string representation of the cpp file."
  (replace-regexp-in-string
   (rx (seq
        line-start (zero-or-more space) "error_msgs << std::endl"
        (zero-or-more space) "\n"
        (zero-or-more space) "<<"))
   "error_msgs <<"
   code))


(defun flycheck-stan-error-msgs--collect-msgs (code)
  "Collect the lines that contain the start of error messages.

error_msgs << std::endl must be cleaned beforehand.
Only the first part of the message within the double quotes
are retained.  The first letter must be an alphabetic letter.
Otherwise, it includes subsequent parts of an error message.
CODE is the string representation of the cpp file.
Do not leave a newline at the very end."
  ;;
  (with-temp-buffer
    (insert code)
    (widen)
    (keep-lines (rx (seq
                     line-start (zero-or-more space) "error_msgs << \""
                     letter
                     (zero-or-more not-newline)))
                (point-min) (point-max))
    ;; Remove anything before and including the first "
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Search-and-Replace.html
    (goto-char (point-min))
    (while (re-search-forward (rx (seq
                                   line-start (zero-or-more space) "error_msgs << \""))
                              nil t)
      (replace-match "" t))
    ;; Remove anything after and including the remaining "
    ;; Retain escaped double quote following backslashes.
    (goto-char (point-min))
    (while (re-search-forward (rx (seq
                                   (not (any "\\\\"))
                                   (group-n 1 "\"" (zero-or-more not-newline))))
                              nil t)
      ;; Replace group 1 only
      (replace-match "" t nil nil 1))
    ;; Drop escape character to turn \" into ".
    ;; We do not want to keep \ before " in this the error_msgs.txt file
    ;; because an escape \ is automatically added when loading this file.
    ;; Keeping \ in the file results in an excess \ breaking pattern
    ;; matching. Work on all instances for a pattern like "}" where
    ;; there are multiple of these in a single line.
    (goto-char (point-min))
    (while (re-search-forward (rx (seq
                                   "\\"
                                   "\""))
                              nil t)
      (replace-match "\"" t))
    ;; Remove trailing \n in the last line if any.
    (while (re-search-forward (rx "\n" buffer-end)
                              nil t)
      (replace-match "" t))
    ;; Remove trailing whitespace and trailing empty lines.
    (let ((delete-trailing-lines t))
      (delete-trailing-whitespace))
    ;; Return the entire string.
    (widen)
    (buffer-substring-no-properties (point-min) (point-max))))


(defun flycheck-stan-error-msgs--clean-lines (code)
  "Clean lines in CODE.

Drop lines starting with Info:.
Drop duplicated lines.
Do not leave a newline at the very end."
  (with-temp-buffer
    (insert code)
    (widen)
    (delete-matching-lines (rx (seq
                                line-start (zero-or-more space) "Info:"
                                (zero-or-more not-newline)))
                           (point-min) (point-max))
    ;; https://emacsredux.com/blog/2014/03/01/a-peek-at-emacs-24-dot-4-delete-duplicate-lines/
    (delete-duplicate-lines (point-min) (point-max))
    ;; Remove trailing \n in the last line if any.
    (while (re-search-forward (rx "\n" buffer-end)
                              nil t)
      (replace-match ""))
    ;; Return the entire string.
    (widen)
    (buffer-substring-no-properties (point-min) (point-max))))


(defun flycheck-stan-error-msgs--create-file (infile outfile)
  "Transform INFILE into a cleaned OUTFILE."
  (let ((code (thread-last (with-temp-buffer
                             (insert-file-contents infile)
                             (buffer-string))
                (flycheck-stan-error-msgs--delete-endl)
                (flycheck-stan-error-msgs--collect-msgs)
                (flycheck-stan-error-msgs--clean-lines))))
    ;; If START is a string, then output that string to the file
    ;; instead of any buffer contents; END is ignored.
    (write-region code
                  nil
                  outfile)))


(provide 'flycheck-stan-error-msgs)

;;;
;;; flycheck-stan-error-msgs.el ends here
