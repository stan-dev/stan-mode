;;; test-eldoc-stan.el --- A buttercup test suite for eldoc-stan -*- lexical-binding: t; -*-

;; Author: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; URL: https://github.com/stan-dev/stan-mode/tree/master/eldoc-stan
;; Keywords: help, tools
;; Version: 10.0.0
;; Created: 2019-07-14
;; Package-Requires: ((emacs "25") (stan-mode "10.0.0"))

;; This file is not part of GNU Emacs.

;;; License:
;;
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
;; Load libraries for testing
(require 'buttercup)
;; Library to be tested
(require 'eldoc-stan)

;;; The following tests are for production use functions.
(describe "regexp for string-match"
  (it "detects the position of first left parenthesis"
    (let ((regexp "(")
          (args-all-comma "(int[] y, matrix x, real alpha, vector beta, real phi)")
          (args-with-given "(int[] y | matrix x, real alpha, vector beta, real phi)")
          (args-with-lhs "int[] y ~ (matrix x, real alpha, vector beta, real phi)"))
      ;;
      (expect
       (string-match regexp args-all-comma)
       :to-equal
       0)
      (expect
       (string-match regexp args-with-given)
       :to-equal
       0)
      ;;
      (expect
       (string-match regexp args-with-lhs)
       :to-equal
       10)))
  ;;
  (it "detects an argument separator, either , or | beyond current position"
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Special.html#Regexp-Special
    ;; [ ... ] character alternative
    ;; We want either , or |.
    (let ((regexp "[,|]")
          (args-all-comma "(int[] y, matrix x, real alpha, vector beta, real phi)")
          (args-with-given "(int[] y | matrix x, real alpha, vector beta, real phi)"))
      ;;
      (expect
       (string-match regexp args-all-comma (1+ 0))
       :to-equal
       8)
      ;;
      (expect
       (string-match regexp args-all-comma (1+ 8))
       :to-equal
       18)
      ;;
      (expect
       (string-match regexp args-all-comma (1+ 18))
       :to-equal
       30)
      ;;
      (expect
       (string-match regexp args-with-given (1+ 0))
       :to-equal
       9)
      ;;
      (expect
       (string-match regexp args-with-given (1+ 9))
       :to-equal
       19)
      ;;
      (expect
       (string-match regexp args-with-given (1+ 19))
       :to-equal
       31)))
  ;;
  (it "detects the beginning of the argument right after the current separator"
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Special.html#Regexp-Special
    ;; [ ... ] character alternative
    ;; [^ ... ] complemented character alternative
    ;; We want to skip the current separator and spaces.
    ;; Be mindful of the very first ( and very last ).
    (let ((regexp "[^ ,|()]")
          (args-all-comma "(int[] y, matrix x, real alpha, vector beta, real phi)")
          (args-with-given "(int[] y | matrix x, real alpha, vector beta, real phi)"))
      ;;
      (expect
       (string-match regexp args-all-comma 0)
       :to-equal
       1)
      ;;
      (expect
       (string-match regexp args-all-comma 8)
       :to-equal
       10)
      ;;
      (expect
       (string-match regexp args-all-comma 43)
       :to-equal
       45)
      ;;
      (expect
       (string-match regexp args-with-given 0)
       :to-equal
       1)
      ;;
      (expect
       (string-match regexp args-with-given 9)
       :to-equal
       11)
      ;;
      (expect
       (string-match regexp args-with-given 44)
       :to-equal
       46)))
  ;;
  (it "detects the end of the current argument starting at its beginning"
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Special.html#Regexp-Special
    ;; [ ... ] character alternative
    ;; We want to detect the next separator or the very last ).
    ;; For |, we want the position of the SPC right before |.
    (let ((regexp "[,)]\\| |")
          (args-all-comma "(int[] y, matrix x, real alpha, vector beta, real phi)")
          (args-with-given "(int[] y | matrix x, real alpha, vector beta, real phi)"))
      ;;
      (expect
       (string-match regexp args-all-comma 1)
       :to-equal
       8)
      ;;
      (expect
       (string-match regexp args-all-comma 10)
       :to-equal
       18)
      ;;
      (expect
       (string-match regexp args-all-comma 45)
       :to-equal
       53)
      ;;
      (expect
       (string-match regexp args-with-given 1)
       :to-equal
       8)
      ;;
      (expect
       (string-match regexp args-with-given 11)
       :to-equal
       19)
      ;;
      (expect
       (string-match regexp args-with-given 46)
       :to-equal
       54))))


(describe "eldoc-stan--substring-propertize"
  (let* ((arguments-1 "(int[] y, matrix x, real alpha, vector beta, real phi)")
         (before1 "(")
         (substr1 "int[] y")
         (after1 ", matrix x, real alpha, vector beta, real phi)")
         ;;
         (arguments-2 "(int[] y | matrix x, real alpha, vector beta, real phi)")
         (before2 "(int[] y | ")
         (substr2 "matrix x")
         (after2 ", real alpha, vector beta, real phi)"))
    ;;
    (it "returns the same string with containing a propertized substring"
      (expect
       (equal-including-properties
        (eldoc-stan--substring-propertize
         arguments-1 1 8
         'face 'eldoc-highlight-function-argument)
        (concat before1
                (propertize substr1
                            'face 'eldoc-highlight-function-argument)
                after1))
       :to-be-truthy)
      ;;
      (expect
       (equal-including-properties
        (eldoc-stan--substring-propertize
         arguments-2 11 19
         'face 'eldoc-highlight-function-argument)
        (concat before2
                (propertize substr2
                            'face 'eldoc-highlight-function-argument)
                after2))
       :to-be-truthy))))


(describe "eldoc-stan--format-arguments-string"
  (let* ((arguments-1 "(int[] y, matrix x, real alpha, vector beta, real phi)")
         (arguments-2 "(int[] y | matrix x, real alpha, vector beta, real phi)"))
    ;;
    (it "performs add-text-properties to the right argument within ()"
      (expect
       (equal-including-properties
        (eldoc-stan--format-arguments-string arguments-1 1)
        (eldoc-stan--substring-propertize
         arguments-1 1 8
         'face 'eldoc-highlight-function-argument))
       :to-be-truthy)
      ;;
      (expect
       (equal-including-properties
        (eldoc-stan--format-arguments-string arguments-1 2)
        (eldoc-stan--substring-propertize
         arguments-1 10 18
         'face 'eldoc-highlight-function-argument))
       :to-be-truthy)
      ;;
      (expect
       (equal-including-properties
        (eldoc-stan--format-arguments-string arguments-1 5)
        (eldoc-stan--substring-propertize
         arguments-1 45 53
         'face 'eldoc-highlight-function-argument))
       :to-be-truthy))
    ;;
    (it "ignores an out-of-upper-bound value"
      (expect
       (equal-including-properties
        (eldoc-stan--format-arguments-string arguments-1 6)
        arguments-1)
       :to-be-truthy))
    ;;
    (it "handles | separating argument correctly"
      (expect
       (equal-including-properties
        (eldoc-stan--format-arguments-string arguments-2 1)
        (eldoc-stan--substring-propertize
         arguments-2 1 8
         'face 'eldoc-highlight-function-argument))
       :to-be-truthy)
      ;;
      (expect
       (equal-including-properties
        (eldoc-stan--format-arguments-string arguments-2 2)
        (eldoc-stan--substring-propertize
         arguments-2 11 19
         'face 'eldoc-highlight-function-argument))
       :to-be-truthy)
      ;;
      (expect
       (equal-including-properties
        (eldoc-stan--format-arguments-string arguments-2 5)
        (eldoc-stan--substring-propertize
         arguments-2 46 54
         'face 'eldoc-highlight-function-argument))
       :to-be-truthy))))


(provide 'test-eldoc-stan)
;;; test-eldoc-stan.el ends here
