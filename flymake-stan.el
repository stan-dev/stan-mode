;;; flymake-stan.el --- A flymake handler for stan files.

;; Copyright (C) 2012, 2013  Jeffrey Arnold
;;
;; Author: Jeffrey Arnold <jeffrey.arnold@gmail.com>
;; URL: http://github.com/stan-dev/stan-mode
;; Keywords: languanges
;; Version: 2.0.0
;; Created: 2012-08-18
;; Package-Requires: ((flymake "0.3"))

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

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
;; Add flymake support for stan.
;;
;; Usage:
;;   (require 'flymake-stan)
;;
;; Stanc must be compiled, and either be on the PATH or either `stan-stanc-path` 
;; or `flymake-stan-stanc-path` set to the location of the executable.

;;; Code:
(require 'flymake)
(require 'stan-mode)

(defvar flymake-stan-stanc-path
  stan-stanc-path
  "Stanc executable to use when running flymake")

(defvar flymake-stan-temp-output-file-name nil
  "Name of the temporary output file produced by stanc when running flymake")

(defun flymake-stan-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (setq flymake-stan-temp-output-file-name
          (make-temp-file "flymake-stan-" nil ".cpp"))
    (list flymake-stan-stanc-path
          (list (concat "--o=" flymake-stan-temp-output-file-name) local-file))))

(defun flymake-stan-cleanup ()
  (flymake-safe-delete-file flymake-stan-temp-output-file-name)
  (flymake-simple-cleanup))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.stan$"
              flymake-stan-init
              flymake-stan-cleanup
              flymake-get-real-file-name)
            flymake-allowed-file-name-masks))

;; This is needed. Otherwise the non-zero return code by
;; stanc causes an error.
;; Solution from http://pastebin.com/2Pp4bj9p
;; TODO: make it local to this mode
(defadvice flymake-post-syntax-check
  (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)

(add-hook 'stan-mode-hook
	  (lambda () (flymake-mode 1)))

(provide 'flymake-stan)

;;; flymake-stan.el ends here
