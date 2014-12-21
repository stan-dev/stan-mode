;;; flycheck-stan.el --- Add Stan support for Flycheck

;; Copyright (C) 2014  Jeffrey Arnold

;; Author: Jeffrey Arnold <jeffrey.arnold@gmail.com>,
;; Maintainer: Jeffrey Arnold <jeffrey.arnold@gmail.com>,
;; URL: http://github.com/stan-dev/stan-mode
;; Keywords: languanges
;; Version: 0.0.1
;; Created: 2014-12-19
;; Package-Requires: ((flycheck "0.16.0") (stan-mode "3.0.0"))

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
;; Add a Stan syntax checker to Flycheck which uses stanc.
;;
;;; Code:
(require 'flycheck)
(require 'stan-mode)

(defcustom stan-stanc-bin
  (if (member system-type '(windows-nt cygwin ms-dos))
      "stanc.exe"
    "stanc")
  "Path to stanc executable

This can also be just the name of the stanc executable if it is on the PATH.
"
  :type 'string
  :group 'flycheck-stan)

(flycheck-define-checker stan-stanc
  "A Stan syntax checker using stanc

See http://mc-stan.org/cmdstan.html"
  :command (stan-stanc-bin source)
  :error-patterns
  ((error
    line-start "Input file=" (file-name) "\n"
    (zero-or-more anything)
    "SYNTAX ERROR" (one-or-more not-newline) "\n"
    (message (one-or-more anything))
    "ERROR at line " line
    ))
  :modes stan-mode)

(add-to-list 'flycheck-checkers 'stan-stanc)

(provide 'flycheck-stan)

;;; flycheck-stan.el ends here
