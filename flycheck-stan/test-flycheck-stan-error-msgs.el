;;; test-flycheck-stan-error-msgs.el --- A buttercup test suite for flycheck-stan-error-msgs -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Kazuki Yoshida

;; Author: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; URL: http://github.com/stan-dev/stan-mode/flycheck-stan
;; Keywords: languages
;; Version: 10.0.0
;; Created: 2019-07-26
;; Package-Requires: ((emacs "24") (flycheck "0.16.0") (stan-mode "10.0.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; emacs-buttercup
;; Behavior-Driven Emacs Lisp Testing
;;  https://github.com/jorgenschaefer/emacs-buttercup
;; Writing Tests
;;  https://github.com/jorgenschaefer/emacs-buttercup/blob/master/docs/writing-tests.md

;;

;;; Code:
(require 'buttercup)
(require 'flycheck-stan-error-msgs)


;; Record the directory of this file up front.
;; This does not work within `it'.
(defvar test-flycheck-stan-error-msgs-dir (file-name-directory
                                           (or load-file-name buffer-file-name)))


(describe "flycheck-stan-error-msgs--delete-endl"
  (it "removes << std::endl from the beginning of error_msgs stream"
    (expect
     (flycheck-stan-error-msgs--delete-endl
      "    error_msgs << std::endl
               << \"Error (fatal):  Use of lp__ is no longer supported.\"
               << std::endl
               << \"  Use target += ... statement to increment log density.\"
               << std::endl;
    return false;")
     :to-equal
     "error_msgs << \"Error (fatal):  Use of lp__ is no longer supported.\"
               << std::endl
               << \"  Use target += ... statement to increment log density.\"
               << std::endl;
    return false;")))


(describe "flycheck-stan-error-msgs--collect-msgs"
  (it "collects correct lines only"
    (expect
     (flycheck-stan-error-msgs--collect-msgs
      "error_msgs << \"Error (fatal):  Use of lp__ is no longer supported.\"
               << std::endl
               << \"  Use target += ... statement to increment log density.\"
               << std::endl;
    return false;")
     :to-equal
     "Error (fatal):  Use of lp__ is no longer supported."))
  ;;
  (it "handles a single double quote within a string correctly"
    (expect
     (flycheck-stan-error-msgs--collect-msgs
      "error_msgs << \"Variable \\\"\" << name << '\"' << \" does not exist.\"
               << std::endl;")
     :to-equal
     "Variable \""))
  ;;
  (it "handles multiple double quotes within a string correctly"
    (expect
     (flycheck-stan-error-msgs--collect-msgs
      "error_msgs << \"Unexpected open block, missing close block \\\"}\\\"\"
               << \" before keyword \\\"\" << name << \"\\\".\" << std::endl;")
     :to-equal
     "Unexpected open block, missing close block \"}\"")))


(describe "flycheck-stan-error-msgs--clean-lines"
  ;;
  (it "drops lines that start with Info:"
    (expect
     (flycheck-stan-error-msgs--clean-lines
      "Error (fatal):  Use of lp__ is no longer supported.
Unknown variable in assignment
Info: assignment operator <- deprecated
Probability function must end in _lpdf or _lpmf.")
     :to-equal
     "Error (fatal):  Use of lp__ is no longer supported.
Unknown variable in assignment
Probability function must end in _lpdf or _lpmf."))
  ;;
  (it "drops duplicated lines"
    (expect
     (flycheck-stan-error-msgs--clean-lines
      "Error (fatal):  Use of lp__ is no longer supported.
Unknown variable in assignment
Unknown variable in assignment
Probability function must end in _lpdf or _lpmf.
Unknown variable in assignment")
     :to-equal
     "Error (fatal):  Use of lp__ is no longer supported.
Unknown variable in assignment
Probability function must end in _lpdf or _lpmf.")))



(provide 'test-flycheck-stan-error-msgs)
;;;
;;; test-flycheck-stan-error-msgs.el ends here
