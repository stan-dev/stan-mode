;;; test-eldoc-stan.el --- A buttercup test suite for eldoc-stan -*- lexical-binding: t; -*-

;; Author: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; URL: https://github.com/stan-dev/stan-mode/tree/master/eldoc-stan
;; Keywords: help, tools
;; Version: 10.1.0
;; Created: 2019-07-14
;; Package-Requires: ((emacs "25") (stan-mode "10.1.0"))

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
(require 'stan-mode)
(require 'eldoc-stan)

;; Record the directory of this file up front.
;; This does not work within `it'.
(defvar test-eldoc-stan-dir (file-name-directory
                             (or load-file-name buffer-file-name)))

;; Define a new matcher for better reporting
(buttercup-define-matcher-for-binary-function
    :to-equal-including-properties
    equal-including-properties)

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
  ;; index FROM (inclusive) and index TO (exclusive)
  (let* ((arguments-1 "(int[] y, matrix x, real alpha, vector beta, real phi)")
         (before1 "(")
         (substr1 "int[] y")
         (after1 ", matrix x, real alpha, vector beta, real phi)")
         ;;
         (arguments-2 "(int[] y | matrix x, real alpha, vector beta, real phi)")
         (before2 "(int[] y | ")
         (substr2 "matrix x")
         (after2 ", real alpha, vector beta, real phi)")
         ;;
         (arguments-3 "(real[,] a)")
         (before3 "(")
         (substr3 "real[,] a")
         (after3 ")"))
    ;;
    (it "returns the same string with containing a propertized substring"
      (expect
       (eldoc-stan--substring-propertize
        arguments-1 1 8
        'face 'eldoc-highlight-function-argument)
       :to-equal-including-properties
       (concat before1
               (propertize substr1
                           'face 'eldoc-highlight-function-argument)
               after1))
      ;;
      (expect
       (eldoc-stan--substring-propertize
        arguments-2 11 19
        'face 'eldoc-highlight-function-argument)
       :to-equal-including-properties
       (concat before2
               (propertize substr2
                           'face 'eldoc-highlight-function-argument)
               after2))
      ;;
      (expect
       (eldoc-stan--substring-propertize
        arguments-3 1 10
        'face 'eldoc-highlight-function-argument)
       :to-equal-including-properties
       (concat before3
               (propertize substr3
                           'face 'eldoc-highlight-function-argument)
               after3)))))


(describe "eldoc-stan--format-arguments-string"
  (let* ((arguments-1 "(int[] y, matrix x, real alpha, vector beta, real phi)")
         (arguments-2 "(int[] y | matrix x, real alpha, vector beta, real phi)")
         (arguments-3 "(real[,] a)")
         (arguments-4 "(int[...] a)"))
    ;;
    (it "performs add-text-properties to the right argument within ()"
      (expect
       (eldoc-stan--format-arguments-string arguments-1 1)
       :to-equal-including-properties
       (eldoc-stan--substring-propertize
        arguments-1 1 8
        'face 'eldoc-highlight-function-argument))
      ;;
      (expect
       (eldoc-stan--format-arguments-string arguments-1 2)
       :to-equal-including-properties
       (eldoc-stan--substring-propertize
        arguments-1 10 18
        'face 'eldoc-highlight-function-argument))
      ;;
      (expect
       (eldoc-stan--format-arguments-string arguments-1 5)
       :to-equal-including-properties
       (eldoc-stan--substring-propertize
        arguments-1 45 53
        'face 'eldoc-highlight-function-argument)))
    ;;
    (it "ignores an out-of-upper-bound value"
      (expect
       (eldoc-stan--format-arguments-string arguments-1 6)
       :to-equal-including-properties
       arguments-1))
    ;;
    (it "handles | separating argument correctly"
      (expect
       (eldoc-stan--format-arguments-string arguments-2 1)
       :to-equal-including-properties
       (eldoc-stan--substring-propertize
        arguments-2 1 8
        'face 'eldoc-highlight-function-argument))
      ;;
      (expect
       (eldoc-stan--format-arguments-string arguments-2 2)
       :to-equal-including-properties
       (eldoc-stan--substring-propertize
        arguments-2 11 19
        'face 'eldoc-highlight-function-argument))
      ;;
      (expect
       (eldoc-stan--format-arguments-string arguments-2 5)
       :to-equal-including-properties
       (eldoc-stan--substring-propertize
        arguments-2 46 54
        'face 'eldoc-highlight-function-argument)))
    ;;
    (it "returns the original text if out of range"
      (expect
       (eldoc-stan--format-arguments-string arguments-1 6)
       :to-equal-including-properties
       arguments-1)
      ;;
      (expect
       (eldoc-stan--format-arguments-string arguments-2 6)
       :to-equal-including-properties
       arguments-2)
      ;;
      (expect
       (eldoc-stan--format-arguments-string arguments-3 2)
       :to-equal-including-properties
       arguments-3)
      ;;
      (expect
       (eldoc-stan--format-arguments-string arguments-4 2)
       :to-equal-including-properties
       arguments-4))
    ;;
    (it "ignores , in [,] correctly"
      (expect
       (eldoc-stan--format-arguments-string arguments-3 1)
       :to-equal-including-properties
       (eldoc-stan--substring-propertize
        arguments-3 1 10
        'face 'eldoc-highlight-function-argument))
      ;;
      (expect
       (eldoc-stan--format-arguments-string arguments-3 2)
       :to-equal-including-properties
       arguments-3))
    ;;
    (it "ignores ... in [...] correctly"
      (expect
       (eldoc-stan--format-arguments-string arguments-4 1)
       :to-equal-including-properties
       (eldoc-stan--substring-propertize
        arguments-4 1 11
        'face 'eldoc-highlight-function-argument))
      ;;
      (expect
       (eldoc-stan--format-arguments-string arguments-4 2)
       :to-equal-including-properties
       arguments-4))))


(describe "eldoc-stan--function-and-argument"
  ;; Not a pure function. Needs a buffer in stan-mode.
  ;;
  (it "detects the current function and 1-based argument index correctly for normal and cauchy"
    ;; Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
    ;; https://emacs.stackexchange.com/questions/33915/problem-with-save-current-buffer-and-find-file
    (with-current-buffer (find-file-noselect
                          (expand-file-name
                           "../flycheck-stan/examples/example_ok.stan"
                           test-eldoc-stan-dir))
      (read-only-mode +1)
      (stan-mode)
      (eldoc-stan-setup)
      (goto-char (point-min))
      ;;
      (search-forward "mu")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              nil)
      ;;
      (search-forward "normal(")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              '("normal" . 1))
      ;;
      (search-forward ",")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              '("normal" . 2))
      ;;
      (search-forward "tau")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              nil)
      ;;
      (search-forward "cauchy(")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              '("cauchy" . 1))
      ;;
      (search-forward ",")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              '("cauchy" . 2))))
  ;;
  (it "detects the current function and 1-based argument index correctly for normal_* with |"
    ;; Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
    ;; https://emacs.stackexchange.com/questions/33915/problem-with-save-current-buffer-and-find-file
    (with-current-buffer (find-file-noselect
                          (expand-file-name
                           "../stan-language-definitions/examples/highlight-test.stan"
                           test-eldoc-stan-dir))
      (read-only-mode +1)
      (stan-mode)
      (eldoc-stan-setup)
      (goto-char (point-min))
      ;;
      (search-forward "foo = normal_lpdf(")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              '("normal_lpdf" . 1))
      (search-forward "|")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              '("normal_lpdf" . 2))
      (search-forward ",")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              '("normal_lpdf" . 3))
      ;;
      (search-forward "foo = normal_cdf(")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              '("normal_cdf" . 1))
      ;; normal_cdf uses , not |
      ;; https://mc-stan.org/docs/2_24/functions-reference/normal-distribution.html
      (search-forward ",")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              '("normal_cdf" . 2))
      (search-forward ",")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              '("normal_cdf" . 3))
      ;;
      ;;
      (search-forward "foo = normal_lcdf(")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              '("normal_lcdf" . 1))
      (search-forward "|")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              '("normal_lcdf" . 2))
      (search-forward ",")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              '("normal_lcdf" . 3))
      ;;
      ;;
      (search-forward "foo = normal_lccdf(")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              '("normal_lccdf" . 1))
      (search-forward "|")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              '("normal_lccdf" . 2))
      (search-forward ",")
      (expect (eldoc-stan--function-and-argument)
              :to-equal
              '("normal_lccdf" . 3)))))


(describe "eldoc-stan-eldoc-documentation-function"
  ;; Not a pure function. Needs a buffer in stan-mode.
  ;; It detects the current function and the argument position in the buffer,
  ;; looks up the json and highligh appropriate argument.
  ;;
  ;; Here write specs for how individual pieces work together.
  ;;
  (it "returns the current function arguments propertized correctly for normal and cauchy"
    ;; Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
    ;; https://emacs.stackexchange.com/questions/33915/problem-with-save-current-buffer-and-find-file
    (with-current-buffer (find-file-noselect
                          (expand-file-name
                           "../flycheck-stan/examples/example_ok.stan"
                           test-eldoc-stan-dir))
      (read-only-mode +1)
      (stan-mode)
      (eldoc-stan-setup)
      (goto-char (point-min))
      ;;
      (search-forward "mu")
      (expect (eldoc-stan-eldoc-documentation-function)
              :to-equal-including-properties
              nil)
      ;;
      (search-forward "normal(")
      (expect (eldoc-stan-eldoc-documentation-function)
              :to-equal-including-properties
              (eldoc-stan--format-arguments-string "(reals mu, reals sigma)" 1))
      ;;
      (search-forward ",")
      (expect (eldoc-stan-eldoc-documentation-function)
              :to-equal-including-properties
              (eldoc-stan--format-arguments-string "(reals mu, reals sigma)" 2))
      ;;
      (search-forward "tau")
      (expect (eldoc-stan-eldoc-documentation-function)
              :to-equal-including-properties
              nil)
      ;;
      (search-forward "cauchy(")
      (expect (eldoc-stan-eldoc-documentation-function)
              :to-equal-including-properties
              (eldoc-stan--format-arguments-string "(reals mu, reals sigma)" 1))
      ;;
      (search-forward ",")
      (expect (eldoc-stan-eldoc-documentation-function)
              :to-equal-including-properties
              (eldoc-stan--format-arguments-string "(reals mu, reals sigma)" 2))))
  ;;
  (it "fontifies the arguments of multi-type function to_matrix correctly"
    (with-temp-buffer
      (stan-mode)
      (eldoc-stan-setup)
      (insert "to_matrix()")
      (goto-char (point-min))
      (search-forward "(")
      (expect
       (eldoc-stan-eldoc-documentation-function)
       :to-equal
       (concat
        (eldoc-stan--format-arguments-string "(int[,] a)" 1) "\n"
        (eldoc-stan--format-arguments-string "(int[] a, int m, int n)" 1) "\n"
        (eldoc-stan--format-arguments-string "(int[] a, int m, int n, int col_major)" 1) "\n"
        (eldoc-stan--format-arguments-string "(matrix m)" 1) "\n"
        (eldoc-stan--format-arguments-string "(matrix m, int m, int n)" 1) "\n"
        (eldoc-stan--format-arguments-string "(matrix m, int m, int n, int col_major)" 1) "\n"
        (eldoc-stan--format-arguments-string "(real[,] a)" 1) "\n"
        (eldoc-stan--format-arguments-string "(real[] a, int m, int n)" 1) "\n"
        (eldoc-stan--format-arguments-string "(real[] a, int m, int n, int col_major)" 1) "\n"
        (eldoc-stan--format-arguments-string "(row_vector v)" 1) "\n"
        (eldoc-stan--format-arguments-string "(row_vector v, int m, int n)" 1) "\n"
        (eldoc-stan--format-arguments-string "(row_vector v, int m, int n, int col_major)" 1) "\n"
        (eldoc-stan--format-arguments-string "(vector v)" 1) "\n"
        (eldoc-stan--format-arguments-string "(vector v, int m, int n)" 1) "\n"
        (eldoc-stan--format-arguments-string "(vector v, int m, int n, int col_major)" 1))))))


(provide 'test-eldoc-stan)
;;; test-eldoc-stan.el ends here
