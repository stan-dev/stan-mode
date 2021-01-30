;;; test-ac-stan.el --- A buttercup test suite for ac-stan -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kazuki Yoshida

;; Author: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; URL: https://github.com/stan-dev/stan-mode/tree/master/ac-stan
;; Keywords: languages
;; Version: 10.2.1
;; Created: 2019-07-26
;; Package-Requires: ((emacs "24") (auto-complete "1.4.0") (stan-mode "10.2.1") (stan-snippets "10.2.1"))

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
(require 'ac-stan)

;; Record package directory
(defvar test-ac-stan--package-directory
  (file-name-directory
   (or load-file-name (buffer-file-name))))


(describe "ac-dictionary-directories"
  (it "contains ac-stan/ac-dict directory"
    (member
     (expand-file-name "ac-dict"
                       test-ac-stan--package-directory)
     ac-dictionary-directories)))


(describe "ac-stan-ac-mode-setup"
  (before-each
    ;; Make `yas-minor-mode-on' ignore temp-buffer
    (setq yas-dont-activate-functions
          '(minibufferp
            ;; yas-temp-buffer-p
            )))
  (it "turns on auto-complete-mode"
    (with-temp-buffer
      (stan-mode)
      (ac-stan-ac-mode-setup)
      (expect
       auto-complete-mode
       :to-be-truthy)))
  (it "turns on yas-minor-mode"
    (with-temp-buffer
      (stan-mode)
      (ac-stan-ac-mode-setup)
      (expect
       yas-minor-mode
       :to-be-truthy)))
  (it "sets ac-sources correctly"
    (with-temp-buffer
      (stan-mode)
      (ac-stan-ac-mode-setup)
      (expect
       ac-sources
       :to-equal
       '(ac-source-imenu
         ac-source-yasnippet
         ac-source-dictionary)))))


(describe "autocomplete in stan-mode"
  (before-each
    ;; Make `yas-minor-mode-on' ignore temp-buffer
    (setq yas-dont-activate-functions
          '(minibufferp
            ;; yas-temp-buffer-p
            )))
  (it "populates ac-candidates correctly after norm"
    (with-temp-buffer
      (stan-mode)
      (ac-stan-ac-mode-setup)
      (insert "norm")
      (auto-complete)
      ;;
      (expect
       (member "normal"
               ac-candidates)
       :to-be-truthy)
      (expect
       (member "normal_lpdf"
               ac-candidates)
       :to-be-truthy))))


(provide 'test-ac-stan)
;;; test-ac-stan.el ends here
