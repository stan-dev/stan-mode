;;; test-flycheck-stan.el --- A buttercup test suite for flycheck-stan -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kazuki Yoshida

;; Author: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; URL: http://github.com/stan-dev/stan-mode/tree/master/flycheck-stan
;; Keywords: languages
;; Version: 10.0.0
;; Created: 2019-07-26
;; Package-Requires: ((emacs "24.3") (flycheck "0.16.0") (stan-mode "10.0.0"))

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
;; Defines :to-match-with-group matcher.
(require 'flycheck-buttercup)
(require 'flycheck-stan)
;; For cl-mapcar, which can map over multiple sequences.
(require 'cl-lib)


;; Record the directory of this file up front.
;; This does not work within `it'.
(defvar test-flycheck-stan-dir (file-name-directory
                                (or load-file-name buffer-file-name)))


;;;
;;; Helper functions
(defun test-flycheck-stan--get-string-from-file (filepath)
  "Return the contents of FILEPATH as a string.

Adopted from http://ergoemacs.org/emacs/elisp_read_file_content.html"
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))


;;;
;;; Define regexp with informative names.
;; OBSOLETE. These were used in the initial attempt using
;; :error-patterns.  However, these patterns are still useful
;; as demonstrations of the enhanced `rx' patterns used in
;; `flycheck-rx-to-string'.  Thus, they are kept.

;; Errors that prevent compilation
(defvar test-flycheck-stan--rx-error-with-line-column
  '(seq ;; Need to use seq instead of error.
    (message
     line-start "SYNTAX ERROR, MESSAGE(S) FROM PARSER:" "\n"
     (one-or-more (or not-newline "\n" "\r"))
     "error in " "'" (file-name) "'"
     " at line " line ", column " column "\n"
     (one-or-more (or not-newline "\n" "\r"))
     ;; To avoid trailing empty lines.
     (not (any whitespace "\n" "\r"))))
  "An `rx' regexp for `stanc' error with `line' and `column' information.
Note that the file name is captured from the message.")

(defvar test-flycheck-stan--rx-error-with-line-only
  '(seq ;; Need to use seq instead of error.
    line-start "Input file=" (file-name) "\n"
    (one-or-more (or not-newline "\n" "\r"))
    (message
     line-start "PARSER FAILED TO PARSE INPUT COMPLETELY"
     (one-or-more (or not-newline "\n" "\r"))
     "STOPPED AT LINE " line ":"
     (one-or-more (or not-newline "\n" "\r"))
     ;; To avoid trailing empty lines.
     (not (any whitespace "\n" "\r"))))
  "An `rx' regexp for parser failure with `line' only.")

(defvar test-flycheck-stan--rx-error-no-include-file
  '(seq ;; Need to use seq instead of error.
    line-start "Input file=" (file-name) "\n"
    (one-or-more (or not-newline "\n" "\r"))
    (message
     line-start "could not find include file"
     (one-or-more (or not-newline "\n" "\r"))
     ;; To avoid trailing empty lines.
     (not (any whitespace "\n" "\r")))))

;; Warnings that should prompt fixing programs.
(defvar test-flycheck-stan--rx-warning-assignment
  '(seq ;; Need to use seq instead of error.
    line-start "Input file=" (file-name) "\n"
    (minimal-match (one-or-more (or not-newline "\n" "\r")))
    (message
     line-start "Info: assignment operator <- deprecated"
     (zero-or-more not-newline))))

(defvar test-flycheck-stan--rx-warning-comments
  '(seq ;; Need to use seq instead of error.
    line-start "Input file=" (file-name) "\n"
    (minimal-match (one-or-more (or not-newline "\n" "\r")))
    (message
     line-start "Info: Comments beginning with # are deprecated."
     (zero-or-more not-newline))))

(defvar test-flycheck-stan--rx-warning-deprecated-log-function
  '(seq ;; Need to use seq instead of error.
    line-start "Input file=" (file-name) "\n"
    (minimal-match (one-or-more (or not-newline "\n" "\r")))
    (message
     line-start "Info: Deprecated function"
     (zero-or-more not-newline))))

(defvar test-flycheck-stan--rx-warning-deprecated-non-log-function
  '(seq ;; Need to use seq instead of error.
    line-start "Input file=" (file-name) "\n"
    (minimal-match (one-or-more (or not-newline "\n" "\r")))
    (message
     line-start "Info:" (zero-or-more not-newline)
     "(" (zero-or-more not-newline)
     ")" (zero-or-more not-newline)
     "is deprecated" (zero-or-more not-newline))))

(defvar test-flycheck-stan--rx-warning-jacobian
  '(seq ;; Need to use seq instead of error.
    line-start "Input file=" (file-name) "\n"
    (minimal-match (one-or-more (or not-newline "\n" "\r")))
    (message
     line-start "Info:" "\n"
     "Left-hand side of sampling statement"
     (zero-or-more not-newline))))


;;;
;;; Tests that do not require the `stanc' binary.
(describe "flycheck-stan-enhance-rx-buffer-locally"
  (it "works on a non-enhanced rx-constituents"
    (let* ((car-rx-constituents (mapcar #'car rx-constituents)))
      (expect
       (memq 'line car-rx-constituents)
       :not :to-be-truthy)
      (expect
       (memq 'column car-rx-constituents)
       :not :to-be-truthy)
      (expect
       (memq 'file-name car-rx-constituents)
       :not :to-be-truthy)
      (expect
       (memq 'message car-rx-constituents)
       :not :to-be-truthy)
      (expect
       (memq 'id car-rx-constituents)
       :not :to-be-truthy)))
  ;;
  (it "enhances rx-constituents with flycheck specific elements"
    ;; Run it to enhance `rx-constituents'.
    (flycheck-stan-enhance-rx-buffer-locally)
    ;;
    (let* ((car-rx-constituents (mapcar #'car rx-constituents)))
      (expect
       (memq 'line car-rx-constituents)
       :to-be-truthy)
      (expect
       (memq 'column car-rx-constituents)
       :to-be-truthy)
      (expect
       (memq 'file-name car-rx-constituents)
       :to-be-truthy)
      (expect
       (memq 'message car-rx-constituents)
       :to-be-truthy)
      (expect
       (memq 'id car-rx-constituents)
       :to-be-truthy))))


(describe "Error pattern handling (past attempt using :error-patterns)"
  ;;
  (it "captures a misspelled type correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_error_misspelled_type.stancout.txt"
                            test-flycheck-stan-dir)))
           (regexp (flycheck-rx-to-string
                    test-flycheck-stan--rx-error-with-line-column)))
      ;; file-name
      (expect regexp
              :to-match-with-group
              error-message
              1 "examples/example_error_misspelled_type.stan")
      ;; line
      (expect regexp
              :to-match-with-group
              error-message
              2 "9")
      ;; column
      (expect regexp
              :to-match-with-group
              error-message
              3 "2")
      ;; message
      (expect regexp
              :to-match-with-group
              error-message
              4 "SYNTAX ERROR, MESSAGE(S) FROM PARSER:
 error in 'examples/example_error_misspelled_type.stan' at line 9, column 2
  -------------------------------------------------
     7: }
     8: parameters {
     9:   rear mu;
         ^
    10:   real<lower=0> tau;
  -------------------------------------------------

PARSER EXPECTED: <one of the following:
  a variable declaration, beginning with type,
      (int, real, vector, row_vector, matrix, unit_vector,
       simplex, ordered, positive_ordered,
       corr_matrix, cov_matrix,
       cholesky_corr, cholesky_cov
  or '}' to close variable declarations>")
      ;; id
      (expect regexp
              :to-match-with-group
              error-message
              5 nil)))
  ;;
  (it "captures a function that needs | correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_error_not_conditional.stancout.txt"
                            test-flycheck-stan-dir)))
           (regexp (flycheck-rx-to-string
                    test-flycheck-stan--rx-error-with-line-column)))
      ;; file-name
      (expect regexp
              :to-match-with-group
              error-message
              1 "examples/example_error_not_conditional.stan")
      ;; line
      (expect regexp
              :to-match-with-group
              error-message
              2 "19")
      ;; column
      (expect regexp
              :to-match-with-group
              error-message
              3 "27")
      ;; message
      (expect regexp
              :to-match-with-group
              error-message
              4 "SYNTAX ERROR, MESSAGE(S) FROM PARSER:
Probabilty functions with suffixes _lpdf, _lpmf, _lcdf, and _lccdf,
require a vertical bar (|) between the first two arguments.
 error in 'examples/example_error_not_conditional.stan' at line 19, column 27
  -------------------------------------------------
    17: model {
    18:   mu ~ normal(0, 10);
    19:   target += cauchy_lpdf(tau, 0, 10);
                                  ^
    20:   eta ~ normal(0, 1); // implies theta ~ normal(mu, tau)
  -------------------------------------------------

PARSER EXPECTED: \"|\"")
      ;; id
      (expect regexp
              :to-match-with-group
              error-message
              5 nil)))
  ;;
  (it "captures an undefined function correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_error_undefined_function.stancout.txt"
                            test-flycheck-stan-dir)))
           (regexp (flycheck-rx-to-string
                    test-flycheck-stan--rx-error-with-line-column)))
      ;; file-name
      (expect regexp
              :to-match-with-group
              error-message
              1 "examples/example_error_undefined_function.stan")
      ;; line
      (expect regexp
              :to-match-with-group
              error-message
              2 "18")
      ;; column
      (expect regexp
              :to-match-with-group
              error-message
              3 "22")
      ;; message
      (expect regexp
              :to-match-with-group
              error-message
              4 "SYNTAX ERROR, MESSAGE(S) FROM PARSER:
Probability function must end in _lpdf or _lpmf. Found distribution family = normall with no corresponding probability function normall_lpdf, normall_lpmf, or normall_log
 error in 'examples/example_error_undefined_function.stan' at line 18, column 22
  -------------------------------------------------
    16: }
    17: model {
    18:   mu ~ normall(0, 10);
                             ^
    19:   tau ~ cauchy(0, 10);
  -------------------------------------------------")
      ;; id
      (expect regexp
              :to-match-with-group
              error-message
              5 nil)))
  ;;
  (it "captures an undefined include file correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_error_undefined_include_file.stancout.txt"
                            test-flycheck-stan-dir)))
           (regexp (flycheck-rx-to-string
                    test-flycheck-stan--rx-error-no-include-file)))
      ;; file-name
      (expect regexp
              :to-match-with-group
              error-message
              1 "examples/example_error_undefined_include_file.stan")
      ;; line
      (expect regexp
              :to-match-with-group
              error-message
              2 nil)
      ;; column
      (expect regexp
              :to-match-with-group
              error-message
              3 nil)
      ;; message
      (expect regexp
              :to-match-with-group
              error-message
              ;; \n to protect trailing white spaces.
              4 "could not find include file no_such_file.stan in the following directories:")
      ;; id
      (expect regexp
              :to-match-with-group
              error-message
              5 nil)))
  ;;
  (it "captures an undefined variable correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_error_undefined_variable.stancout.txt"
                            test-flycheck-stan-dir)))
           (regexp (flycheck-rx-to-string
                    test-flycheck-stan--rx-error-with-line-column)))
      ;; file-name
      (expect regexp
              :to-match-with-group
              error-message
              1 "examples/example_error_undefined_variable.stan")
      ;; line
      (expect regexp
              :to-match-with-group
              error-message
              2 "15")
      ;; column
      (expect regexp
              :to-match-with-group
              error-message
              3 "12")
      ;; message
      (expect regexp
              :to-match-with-group
              error-message
              4 "SYNTAX ERROR, MESSAGE(S) FROM PARSER:
Variable \"mu\" does not exist.
 error in 'examples/example_error_undefined_variable.stan' at line 15, column 12
  -------------------------------------------------
    13: transformed parameters {
    14:   vector[J] theta;
    15:   theta = mu + tau * eta;
                   ^
    16: }
  -------------------------------------------------")
      ;; id
      (expect regexp
              :to-match-with-group
              error-message
              5 nil)))
  ;;
  (it "captures a misspelled block correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_failure_misspelled_block.stancout.txt"
                            test-flycheck-stan-dir)))
           (regexp (flycheck-rx-to-string
                    test-flycheck-stan--rx-error-with-line-only)))
      ;; file-name
      (expect regexp
              :to-match-with-group
              error-message
              1 "examples/example_failure_misspelled_block.stan")
      ;; line
      (expect regexp
              :to-match-with-group
              error-message
              2 "8")
      ;; column
      (expect regexp
              :to-match-with-group
              error-message
              3 nil)
      ;; message
      (expect regexp
              :to-match-with-group
              error-message
              ;; \n to protect the trailing white space.
              4 "PARSER FAILED TO PARSE INPUT COMPLETELY
STOPPED AT LINE 8: \npparameters {
  real mu;
  real<lower=0> tau;
  vector[J] eta;
}
transformed parameters {
  vector[J] theta;
  theta = mu + tau * eta;
}
model {
  mu ~ normal(0, 10);
  tau ~ cauchy(0, 10);
  eta ~ normal(0, 1); // implies theta ~ normal(mu, tau)
  y ~ normal(theta, sigma);
}")
      ;; id
      (expect regexp
              :to-match-with-group
              error-message
              5 nil))))


(describe "Info pattern handling (past attempt using :error-patterns)"
  ;;
  (it "captures the first deprecated assignment correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_info_deprecated_assignment.stancout.txt"
                            test-flycheck-stan-dir)))
           (regexp (flycheck-rx-to-string
                    test-flycheck-stan--rx-warning-assignment)))
      ;; file-name
      (expect regexp
              :to-match-with-group
              error-message
              1 "examples/example_info_deprecated_assignment.stan")
      ;; line
      (expect regexp
              :to-match-with-group
              error-message
              2 nil)
      ;; column
      (expect regexp
              :to-match-with-group
              error-message
              3 nil)
      ;; message
      (expect regexp
              :to-match-with-group
              error-message
              ;; \n to protect the trailing white space.
              4 "Info: assignment operator <- deprecated in the Stan language; use = instead.")
      ;; id
      (expect regexp
              :to-match-with-group
              error-message
              5 nil)))
  ;;
  (it "captures the first deprecated comment correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_info_deprecated_comments.stancout.txt"
                            test-flycheck-stan-dir)))
           (regexp (flycheck-rx-to-string
                    test-flycheck-stan--rx-warning-comments)))
      ;; file-name
      (expect regexp
              :to-match-with-group
              error-message
              1 "examples/example_info_deprecated_comments.stan")
      ;; line
      (expect regexp
              :to-match-with-group
              error-message
              2 nil)
      ;; column
      (expect regexp
              :to-match-with-group
              error-message
              3 nil)
      ;; message
      (expect regexp
              :to-match-with-group
              error-message
              ;; \n to protect the trailing white space.
              4 "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments.")
      ;; id
      (expect regexp
              :to-match-with-group
              error-message
              5 nil)))
  ;;
  (it "captures the first deprecated *_log function correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_info_deprecated_functions.stancout.txt"
                            test-flycheck-stan-dir)))
           (regexp (flycheck-rx-to-string
                    test-flycheck-stan--rx-warning-deprecated-log-function)))
      ;; file-name
      (expect regexp
              :to-match-with-group
              error-message
              1 "examples/example_info_deprecated_functions.stan")
      ;; line
      (expect regexp
              :to-match-with-group
              error-message
              2 nil)
      ;; column
      (expect regexp
              :to-match-with-group
              error-message
              3 nil)
      ;; message
      (expect regexp
              :to-match-with-group
              error-message
              ;; \n to protect the trailing white space.
              4 "Info: Deprecated function 'normal_log'; please replace suffix '_log' with '_lpdf' for density functions or '_lpmf' for mass functions")
      ;; id
      (expect regexp
              :to-match-with-group
              error-message
              5 nil)))
  ;;
  (it "captures the first deprecated non-*_log function correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_info_deprecated_functions.stancout.txt"
                            test-flycheck-stan-dir)))
           (regexp (flycheck-rx-to-string
                    test-flycheck-stan--rx-warning-deprecated-non-log-function)))
      ;; file-name
      (expect regexp
              :to-match-with-group
              error-message
              1 "examples/example_info_deprecated_functions.stan")
      ;; line
      (expect regexp
              :to-match-with-group
              error-message
              2 nil)
      ;; column
      (expect regexp
              :to-match-with-group
              error-message
              3 nil)
      ;; message
      (expect regexp
              :to-match-with-group
              error-message
              ;; \n to protect the trailing white space.
              4 "Info: increment_log_prob(...); is deprecated and will be removed in the future.")
      ;; id
      (expect regexp
              :to-match-with-group
              error-message
              5 nil)))
  ;;
  (it "captures the first Jacobian info"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_info_jacobian.stancout.txt"
                            test-flycheck-stan-dir)))
           (regexp (flycheck-rx-to-string
                    '(seq ;; Need to use seq instead of error.
                      line-start "Input file=" (file-name) "\n"
                      (minimal-match (one-or-more (or not-newline "\n" "\r")))
                      (message
                       line-start "Info:" "\n"
                       "Left-hand side of sampling statement"
                       (zero-or-more not-newline))))))
      ;; file-name
      (expect regexp
              :to-match-with-group
              error-message
              1 "examples/example_info_jacobian.stan")
      ;; line
      (expect regexp
              :to-match-with-group
              error-message
              2 nil)
      ;; column
      (expect regexp
              :to-match-with-group
              error-message
              3 nil)
      ;; message
      (expect regexp
              :to-match-with-group
              error-message
              ;; \n to protect the trailing white space.
              4 "Info:
Left-hand side of sampling statement (~) may contain a non-linear transform of a parameter or local variable.")
      ;; id
      (expect regexp
              :to-match-with-group
              error-message
              5 nil))))


(describe "flycheck-stan--list-of-strings-from-file"
  (let ((error-msgs (flycheck-stan--list-of-strings-from-file
                     (expand-file-name
                      "./error_msgs.txt"
                      test-flycheck-stan-dir)))
        (error-msgs-str (test-flycheck-stan--get-string-from-file
                         (expand-file-name
                          "./error_msgs.txt"
                          test-flycheck-stan-dir))))
    ;;
    (it "gives a list of strings"
      (expect
       (listp error-msgs)
       :to-be-truthy)
      (expect
       (seq-every-p #'stringp error-msgs)
       :to-be-truthy))
    ;;
    (it "should give a list with as many elements as non-empty lines in file"
      (expect
       (length error-msgs)
       :to-equal
       (with-temp-buffer
         (insert error-msgs-str)
         ;; Need to move back by one to stay at the end
         ;; of last non-empty line.
         (goto-char (1- (point-max)))
         (line-number-at-pos))))))


(describe "flycheck-stan-cleaner"
  ;;
  (it "removes white spaces at the end of lines"
    (expect
     (flycheck-stan-cleaner
      "a  \nb        \nc  \n")
     :to-equal
     "a
b
c"))
  ;;
  (it "removes empty lines at the end"
    (expect
     (flycheck-stan-cleaner
      "a
b
c


")
     :to-equal
     "a
b
c"))
  ;;
  (it "removes empty lines in between"
    (expect
     (flycheck-stan-cleaner
      "a

b


c
")
     :to-equal
     "a
b
c"))
  ;;
  (it "cleans both trailing whitespace and empty lines and add Error:"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_info_composite_with_error.stancout.txt"
                            test-flycheck-stan-dir))))
      (expect
       (flycheck-stan-cleaner
        error-message)
       :to-equal
       ;; No empty lines or trailing whitespace. Error tag.
       "Model name=example_info_composite_with_error_model
Input file=examples/example_info_composite_with_error.stan
Output file=examples/example_info_composite_with_error.cpp
SYNTAX ERROR, MESSAGE(S) FROM PARSER:
Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments.
Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments.
Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments.
Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments.
Info: assignment operator <- deprecated in the Stan language; use = instead.
Info:
Left-hand side of sampling statement (~) may contain a non-linear transform of a parameter or local variable.
If it does, you need to include a target += statement with the log absolute determinant of the Jacobian of the transform.
Left-hand-side of sampling statement:
    stan::math::exp(stan::math::log(mu)) ~ normal(...)
Info: increment_log_prob(...); is deprecated and will be removed in the future.
  Use target += ...; instead.
Info: Deprecated function 'cauchy_log'; please replace suffix '_log' with '_lpdf' for density functions or '_lpmf' for mass functions
Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments.
Info: increment_log_prob(...); is deprecated and will be removed in the future.
  Use target += ...; instead.
Error: Probabilty functions with suffixes _lpdf, _lpmf, _lcdf, and _lccdf,
require a vertical bar (|) between the first two arguments.
 error in 'examples/example_info_composite_with_error.stan' at line 21, column 34
  -------------------------------------------------
    19:   increment_log_prob(cauchy_log(tau, 0, 10));
    20:   eta ~ normal(0, 1); # implies theta ~ normal(mu, tau)
    21:   increment_log_prob(normal_lpdf(y, theta, sigma));
                                         ^
    22: }
  -------------------------------------------------
PARSER EXPECTED: \"|\"")))
  ;;
  (it "adds Error: correctly for undefined include file"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "examples/example_error_and_info_undefined_include_file.stancout.txt"
                            test-flycheck-stan-dir))))
      (expect
       (flycheck-stan-cleaner
        error-message)
       :to-equal
       "Model name=example_error_and_info_undefined_include_file_model
Input file=examples/example_error_and_info_undefined_include_file.stan
Output file=examples/example_error_and_info_undefined_include_file.cpp
Error: could not find include file no_such_file.stan in the following directories:")))
  ;;
  (it "adds Error: correctly for missing error message before error in..."
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "examples/example_error_and_info_composite.stancout.txt"
                            test-flycheck-stan-dir))))
      (expect
       (flycheck-stan-cleaner
        error-message)
       :to-equal
       "Model name=example_error_and_info_composite_model
Input file=examples/example_error_and_info_composite.stan
Output file=examples/example_error_and_info_composite.cpp
SYNTAX ERROR, MESSAGE(S) FROM PARSER:
Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments.
Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments.
Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments.
Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments.
Error:
 error in 'examples/example_error_and_info_composite.stan' at line 9, column 2
  -------------------------------------------------
     7: }
     8: parameters {
     9:   rear mu;
         ^
    10:   // The parser stops at the above line.
  -------------------------------------------------
PARSER EXPECTED: <one of the following:
  a variable declaration, beginning with type,
      (int, real, vector, row_vector, matrix, unit_vector,
       simplex, ordered, positive_ordered,
       corr_matrix, cov_matrix,
       cholesky_corr, cholesky_cov
  or '}' to close variable declarations>")))
  ;;
  (it "adds Error: correctly for a line involving a variable name"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "examples/example_error_and_info_undefined_variable.stancout.txt"
                            test-flycheck-stan-dir))))
      (expect
       (flycheck-stan-cleaner
        error-message)
       :to-equal
       "Model name=example_error_and_info_undefined_variable_model
Input file=examples/example_error_and_info_undefined_variable.stan
Output file=examples/example_error_and_info_undefined_variable.cpp
SYNTAX ERROR, MESSAGE(S) FROM PARSER:
Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments.
Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments.
Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments.
Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments.
Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments.
Error: Variable \"mu\" does not exist.
 error in 'examples/example_error_and_info_undefined_variable.stan' at line 15, column 12
  -------------------------------------------------
    13: transformed parameters {
    14:   vector[J] theta;
    15:   theta = mu + tau * eta;
                   ^
    16: }
  -------------------------------------------------")))
  ;;
  (it "adds Error: correctly for a line that is all capital"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "examples/example_failure_misspelled_block.stancout.txt"
                            test-flycheck-stan-dir))))
      (expect
       (flycheck-stan-cleaner
        error-message)
       :to-equal
       "Model name=example_failure_misspelled_block_model
Input file=examples/example_failure_misspelled_block.stan
Output file=examples/example_failure_misspelled_block.cpp
Error: PARSER FAILED TO PARSE INPUT COMPLETELY
STOPPED AT LINE 8:
pparameters {
  real mu;
  real<lower=0> tau;
  vector[J] eta;
}
transformed parameters {
  vector[J] theta;
  theta = mu + tau * eta;
}
model {
  mu ~ normal(0, 10);
  tau ~ cauchy(0, 10);
  eta ~ normal(0, 1); // implies theta ~ normal(mu, tau)
  y ~ normal(theta, sigma);
}")))
  ;;
  (it "adds Error: correctly for a model name starting with a number"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "examples/01_example_error_number_model_name.stancout.txt"
                            test-flycheck-stan-dir))))
      (expect
       (flycheck-stan-cleaner
        error-message)
       :to-equal
       "Error: model_name must not start with a number or symbol other than _
Error: Could not remove output file=examples/01_example_error_number_model_name.cpp"))))


(describe "flycheck-stan-splitter"
  (it "splits text at Info and error"
    (expect
     (flycheck-stan-splitter
      "Header
Info: a
Error: b
Error:
c")
     :to-equal
     (list
      "Header"
      "Info: a"
      "Error: b"
      "Error:
c")))
  ;;
  (it "splits text at Info and error even without a header"
    (expect
     (flycheck-stan-splitter
      "Info: a
Error: b
Error:
c")
     :to-equal
     (list
      "Info: a"
      "Error: b"
      "Error:
c")))
  ;;
  (it "handles multiple line Info correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_info_composite_with_error.stancout.txt"
                            test-flycheck-stan-dir))))
      (expect
       (flycheck-stan-splitter (flycheck-stan-cleaner
                                error-message))
       :to-equal
       ;; No empty lines or trailing whitespace. Error tag.
       (list
        "Model name=example_info_composite_with_error_model
Input file=examples/example_info_composite_with_error.stan
Output file=examples/example_info_composite_with_error.cpp
SYNTAX ERROR, MESSAGE(S) FROM PARSER:"
        "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
        "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
        "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
        "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
        "Info: assignment operator <- deprecated in the Stan language; use = instead."
        "Info:
Left-hand side of sampling statement (~) may contain a non-linear transform of a parameter or local variable.
If it does, you need to include a target += statement with the log absolute determinant of the Jacobian of the transform.
Left-hand-side of sampling statement:
    stan::math::exp(stan::math::log(mu)) ~ normal(...)"
        "Info: increment_log_prob(...); is deprecated and will be removed in the future.
  Use target += ...; instead."
        "Info: Deprecated function 'cauchy_log'; please replace suffix '_log' with '_lpdf' for density functions or '_lpmf' for mass functions"
        "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
        "Info: increment_log_prob(...); is deprecated and will be removed in the future.
  Use target += ...; instead."
        "Error: Probabilty functions with suffixes _lpdf, _lpmf, _lcdf, and _lccdf,
require a vertical bar (|) between the first two arguments.
 error in 'examples/example_info_composite_with_error.stan' at line 21, column 34
  -------------------------------------------------
    19:   increment_log_prob(cauchy_log(tau, 0, 10));
    20:   eta ~ normal(0, 1); # implies theta ~ normal(mu, tau)
    21:   increment_log_prob(normal_lpdf(y, theta, sigma));
                                         ^
    22: }
  -------------------------------------------------
PARSER EXPECTED: \"|\""))))
  ;;
  (it "splits entire error string into header, infos, and errors"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "examples/example_error_and_info_composite.stancout.txt"
                            test-flycheck-stan-dir))))
      (expect
       (flycheck-stan-splitter (flycheck-stan-cleaner
                                error-message))
       :to-equal
       (list
        "Model name=example_error_and_info_composite_model
Input file=examples/example_error_and_info_composite.stan
Output file=examples/example_error_and_info_composite.cpp
SYNTAX ERROR, MESSAGE(S) FROM PARSER:"
        "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
        "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
        "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
        "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
        "Error:
 error in 'examples/example_error_and_info_composite.stan' at line 9, column 2
  -------------------------------------------------
     7: }
     8: parameters {
     9:   rear mu;
         ^
    10:   // The parser stops at the above line.
  -------------------------------------------------
PARSER EXPECTED: <one of the following:
  a variable declaration, beginning with type,
      (int, real, vector, row_vector, matrix, unit_vector,
       simplex, ordered, positive_ordered,
       corr_matrix, cov_matrix,
       cholesky_corr, cholesky_cov
  or '}' to close variable declarations>"))))
  ;;
  (it "handles a file name that starts with a number correctly"
    ;; This one lacks the header.
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "examples/01_example_error_number_model_name.stancout.txt"
                            test-flycheck-stan-dir))))
      (expect
       (flycheck-stan-splitter (flycheck-stan-cleaner
                                error-message))
       :to-equal
       (list
        "Error: model_name must not start with a number or symbol other than _"
        "Error: Could not remove output file=examples/01_example_error_number_model_name.cpp")))))


(describe "flycheck-stan-convert-message-to-error"
  (it "converts one-line Info correctly"
    (expect
     (flycheck-stan-convert-message-to-error
      "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments"
      'buffer 'stanc "fake_input_file")
     :to-equal
     (flycheck-error-new :buffer 'buffer
                         :checker 'stanc
                         :filename "fake_input_file"
                         :line 0
                         :column nil
                         :message "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments"
                         :level 'info
                         :id nil
                         :group nil)))
  ;;
  (it "converts multi-line Info correctly"
    (expect
     (flycheck-stan-convert-message-to-error
      "Info:
Left-hand side of sampling statement (~) may contain a non-linear transform of a parameter or local variable.
If it does, you need to include a target += statement with the log absolute determinant of the Jacobian of the transform.
Left-hand-side of sampling statement:
    stan::math::exp(stan::math::log(mu)) ~ normal(...)"
      'buffer 'stanc "fake_input_file")
     :to-equal
     (flycheck-error-new :buffer 'buffer
                         :checker 'stanc
                         :filename "fake_input_file"
                         :line 0
                         :column nil
                         :message "Info:
Left-hand side of sampling statement (~) may contain a non-linear transform of a parameter or local variable.
If it does, you need to include a target += statement with the log absolute determinant of the Jacobian of the transform.
Left-hand-side of sampling statement:
    stan::math::exp(stan::math::log(mu)) ~ normal(...)"
                         :level 'info
                         :id nil
                         :group nil)))
  ;;
  (it "converts Error without line/column/filename correctly"
    (expect
     (flycheck-stan-convert-message-to-error
      "Error: could not find include file no_such_file.stan in the following directories:"
      'buffer 'stanc "fake_input_file")
     :to-equal
     (flycheck-error-new :buffer 'buffer
                         :checker 'stanc
                         :filename "fake_input_file"
                         :line 0
                         :column nil
                         :message "Error: could not find include file no_such_file.stan in the following directories:"
                         :level 'error
                         :id nil
                         :group nil)))
  ;;
  (it "converts Error with line only correctly"
    (expect
     (flycheck-stan-convert-message-to-error
      "Error: PARSER FAILED TO PARSE INPUT COMPLETELY
STOPPED AT LINE 8:
pparameters {
  real mu;
  real<lower=0> tau;
  vector[J] eta;
}
transformed parameters {
  vector[J] theta;
  theta = mu + tau * eta;
}
model {
  mu ~ normal(0, 10);
  tau ~ cauchy(0, 10);
  eta ~ normal(0, 1); # implies theta ~ normal(mu, tau)
  y ~ normal(theta, sigma);
}"
      'buffer 'stanc "fake_input_file")
     :to-equal
     (flycheck-error-new :buffer 'buffer
                         :checker 'stanc
                         :filename "fake_input_file"
                         :line 8
                         :column nil
                         :message "Error: PARSER FAILED TO PARSE INPUT COMPLETELY
STOPPED AT LINE 8:
pparameters {
  real mu;
  real<lower=0> tau;
  vector[J] eta;
}
transformed parameters {
  vector[J] theta;
  theta = mu + tau * eta;
}
model {
  mu ~ normal(0, 10);
  tau ~ cauchy(0, 10);
  eta ~ normal(0, 1); # implies theta ~ normal(mu, tau)
  y ~ normal(theta, sigma);
}"
                         :level 'error
                         :id nil
                         :group nil)))
  ;;
  (it "converts Error with file, line, and column correctly"
    (expect
     (flycheck-stan-convert-message-to-error
      "Error: Probability function must end in _lpdf or _lpmf.  Found distribution family = normall with no corresponding probability function normall_lpdf, normall_lpmf, or normall_log
 error in 'examples/example_error_and_info_undefined_function.stan' at line 18, column 22
  -------------------------------------------------
    16: }
    17: model {
    18:   mu ~ normall(0, 10);
                             ^
    19:   tau ~ cauchy(0, 10);
  -------------------------------------------------"
      'buffer 'stanc "fake_input_file")
     :to-equal
     (flycheck-error-new :buffer 'buffer
                         :checker 'stanc
                         :filename "examples/example_error_and_info_undefined_function.stan"
                         :line 18
                         :column 22
                         :message "Error: Probability function must end in _lpdf or _lpmf.  Found distribution family = normall with no corresponding probability function normall_lpdf, normall_lpmf, or normall_log
 error in 'examples/example_error_and_info_undefined_function.stan' at line 18, column 22
  -------------------------------------------------
    16: }
    17: model {
    18:   mu ~ normall(0, 10);
                             ^
    19:   tau ~ cauchy(0, 10);
  -------------------------------------------------"
                         :level 'error
                         :id nil
                         :group nil))))


(describe "flycheck-stan-parser"
  ;; This file uses `flycheck-error-new-at' to create a gold-standard error object.
  ;;  https://github.com/flycheck/flycheck/blob/master/test/specs/test-error-filters.el
  ;; This file also uses `flycheck-error-new' to create a gold-standard error object.
  ;;  https://github.com/flycheck/flycheck/blob/master/test/specs/test-error-parsers.el
  ;; Both are :constructor's for the `flycheck-error' object.
  ;;  https://github.com/flycheck/flycheck/blob/master/flycheck.el#L3255
  ;;  https://www.gnu.org/software/emacs/manual/html_mono/cl.html#Structures
  ;; `flycheck-error-new' requires keyword arguments for all its arguments.
  ;; `flycheck-error-new-at' has a different argument order.
  ;; Access to an element like: (flycheck-error-checker correct-error).
  ;;
  ;; Compare lists of errors using `:to-be-equal-flycheck-errors'.
  ;; This matcher calls `flycheck-error-format', which then calls
  ;; `number-to-string' on the line number. Thus it cannot be nil.
  ;;
  (before-each
    ;; Spying
    ;; (buffer-file-name) is called inside flycheck-stan-parser
    ;; to obtain the file name associated with the current buffer.
    ;; This information is not used as long as the error message
    ;; contains a header with the file name.
    ;; Here we are passing a non-buffer object to flycheck-stan-parser.
    ;; Thus, overriding buff-file-name is necessary to avoid
    ;; unwanted errors.
    (spy-on 'buffer-file-name
            :and-return-value
            "fake_file_name.stan"))
  ;;
  (it "captures a misspelled type correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_error_misspelled_type.stancout.txt"
                            test-flycheck-stan-dir)))
           ;; list of flycheck-style error objects
           (flycheck-errors (flycheck-stan-parser
                             error-message 'stanc 'buffer-object))
           ;; Gold standard object
           ;; (flycheck-error-new-at
           ;;  LINE COLUMN &optional LEVEL MESSAGE &key CHECKER ID GROUP
           ;;  (FILENAME (buffer-file-name)) (BUFFER (current-buffer)))
           ;; LINE and COLUMN are numbers.
           (correct-errors
            (list (flycheck-error-new-at
                   ;; line column level
                   9 2 'error
                   ;; message
                   "Error:
 error in 'examples/example_error_misspelled_type.stan' at line 9, column 2
  -------------------------------------------------
     7: }
     8: parameters {
     9:   rear mu;
         ^
    10:   real<lower=0> tau;
  -------------------------------------------------
PARSER EXPECTED: <one of the following:
  a variable declaration, beginning with type,
      (int, real, vector, row_vector, matrix, unit_vector,
       simplex, ordered, positive_ordered,
       corr_matrix, cov_matrix,
       cholesky_corr, cholesky_cov
  or '}' to close variable declarations>"
                   :checker 'stanc
                   :id nil
                   :group nil
                   :filename
                   "examples/example_error_misspelled_type.stan"
                   :buffer 'buffer-object))))
      (expect
       (length flycheck-errors)
       :to-equal
       (length correct-errors))
      (expect
       flycheck-errors
       :to-be-equal-flycheck-errors
       correct-errors)))
  ;;
  (it "captures a misspelled type correctly even with infos"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_error_and_info_misspelled_type.stancout.txt"
                            test-flycheck-stan-dir)))
           ;; list of flycheck-style error objects
           (flycheck-errors (flycheck-stan-parser
                             error-message 'stanc 'buffer-object))
           ;; Gold standard object
           ;; (flycheck-error-new-at
           ;;  LINE COLUMN &optional LEVEL MESSAGE &key CHECKER ID GROUP
           ;;  (FILENAME (buffer-file-name)) (BUFFER (current-buffer)))
           (correct-errors
            (list
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_error_and_info_misspelled_type.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_error_and_info_misspelled_type.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_error_and_info_misspelled_type.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_error_and_info_misspelled_type.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              9 2 'error
              ;; message
              "Error:
 error in 'examples/example_error_and_info_misspelled_type.stan' at line 9, column 2
  -------------------------------------------------
     7: }
     8: parameters {
     9:   rear mu;
         ^
    10:   real<lower=0> tau;
  -------------------------------------------------
PARSER EXPECTED: <one of the following:
  a variable declaration, beginning with type,
      (int, real, vector, row_vector, matrix, unit_vector,
       simplex, ordered, positive_ordered,
       corr_matrix, cov_matrix,
       cholesky_corr, cholesky_cov
  or '}' to close variable declarations>"
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_error_and_info_misspelled_type.stan"
              :buffer 'buffer-object))))
      (expect
       (length flycheck-errors)
       :to-equal
       (length correct-errors))
      (expect
       flycheck-errors
       :to-be-equal-flycheck-errors
       correct-errors)))
  ;;
  (it "captures a function that needs | correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_error_not_conditional.stancout.txt"
                            test-flycheck-stan-dir)))
           ;; list of flycheck-style error objects
           (flycheck-errors (flycheck-stan-parser
                             error-message 'stanc 'buffer-object))
           ;; Gold standard object
           ;; (flycheck-error-new-at
           ;;  LINE COLUMN &optional LEVEL MESSAGE &key CHECKER ID GROUP
           ;;  (FILENAME (buffer-file-name)) (BUFFER (current-buffer)))
           (correct-errors
            (list (flycheck-error-new-at
                   ;; line column level
                   19 27 'error
                   ;; message
                   "Error: Probabilty functions with suffixes _lpdf, _lpmf, _lcdf, and _lccdf,
require a vertical bar (|) between the first two arguments.
 error in 'examples/example_error_not_conditional.stan' at line 19, column 27
  -------------------------------------------------
    17: model {
    18:   mu ~ normal(0, 10);
    19:   target += cauchy_lpdf(tau, 0, 10);
                                  ^
    20:   eta ~ normal(0, 1); // implies theta ~ normal(mu, tau)
  -------------------------------------------------
PARSER EXPECTED: \"|\""
                   :checker 'stanc
                   :id nil
                   :group nil
                   :filename
                   "examples/example_error_not_conditional.stan"
                   :buffer 'buffer-object))))
      (expect
       (length flycheck-errors)
       :to-equal
       (length correct-errors))
      (expect
       flycheck-errors
       :to-be-equal-flycheck-errors
       correct-errors)))
  ;;
  (it "captures a function that needs | correctly even with infos"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_error_and_info_not_conditional.stancout.txt"
                            test-flycheck-stan-dir)))
           ;; list of flycheck-style error objects
           (flycheck-errors (flycheck-stan-parser
                             error-message 'stanc 'buffer-object))
           ;; Gold standard object
           ;; (flycheck-error-new-at
           ;;  LINE COLUMN &optional LEVEL MESSAGE &key CHECKER ID GROUP
           ;;  (FILENAME (buffer-file-name)) (BUFFER (current-buffer)))
           (correct-errors
            (list
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_error_and_info_not_conditional.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_error_and_info_not_conditional.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_error_and_info_not_conditional.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_error_and_info_not_conditional.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              19 27 'error
              ;; message
              "Error: Probabilty functions with suffixes _lpdf, _lpmf, _lcdf, and _lccdf,
require a vertical bar (|) between the first two arguments.
 error in 'examples/example_error_and_info_not_conditional.stan' at line 19, column 27
  -------------------------------------------------
    17: model {
    18:   mu ~ normal(0, 10);
    19:   target += cauchy_lpdf(tau, 0, 10);
                                  ^
    20:   eta ~ normal(0, 1); # implies theta ~ normal(mu, tau)
  -------------------------------------------------
PARSER EXPECTED: \"|\""
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_error_and_info_not_conditional.stan"
              :buffer 'buffer-object))))
      (expect
       (length flycheck-errors)
       :to-equal
       (length correct-errors))
      (expect
       flycheck-errors
       :to-be-equal-flycheck-errors
       correct-errors)))
  ;;
  (it "captures an undefined function correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_error_undefined_function.stancout.txt"
                            test-flycheck-stan-dir)))
           ;; list of flycheck-style error objects
           (flycheck-errors (flycheck-stan-parser
                             error-message 'stanc 'buffer-object))
           ;; Gold standard object
           ;; (flycheck-error-new-at
           ;;  LINE COLUMN &optional LEVEL MESSAGE &key CHECKER ID GROUP
           ;;  (FILENAME (buffer-file-name)) (BUFFER (current-buffer)))
           (correct-errors
            (list (flycheck-error-new-at
                   ;; line column level
                   18 22 'error
                   ;; message
                   "Error: Probability function must end in _lpdf or _lpmf. Found distribution family = normall with no corresponding probability function normall_lpdf, normall_lpmf, or normall_log
 error in 'examples/example_error_undefined_function.stan' at line 18, column 22
  -------------------------------------------------
    16: }
    17: model {
    18:   mu ~ normall(0, 10);
                             ^
    19:   tau ~ cauchy(0, 10);
  -------------------------------------------------"
                   :checker 'stanc
                   :id nil
                   :group nil
                   :filename
                   "examples/example_error_undefined_function.stan"
                   :buffer 'buffer-object))))
      (expect
       (length flycheck-errors)
       :to-equal
       (length correct-errors))
      (expect
       flycheck-errors
       :to-be-equal-flycheck-errors
       correct-errors)))
  ;;
  (it "captures an undefined function correctly even with infos"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_error_and_info_undefined_function.stancout.txt"
                            test-flycheck-stan-dir)))
           ;; list of flycheck-style error objects
           (flycheck-errors (flycheck-stan-parser
                             error-message 'stanc 'buffer-object))
           ;; Gold standard object
           ;; (flycheck-error-new-at
           ;;  LINE COLUMN &optional LEVEL MESSAGE &key CHECKER ID GROUP
           ;;  (FILENAME (buffer-file-name)) (BUFFER (current-buffer)))
           (correct-errors
            (list
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_error_and_info_undefined_function.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_error_and_info_undefined_function.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_error_and_info_undefined_function.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_error_and_info_undefined_function.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              18 22 'error
              ;; message
              "Error: Probability function must end in _lpdf or _lpmf. Found distribution family = normall with no corresponding probability function normall_lpdf, normall_lpmf, or normall_log
 error in 'examples/example_error_and_info_undefined_function.stan' at line 18, column 22
  -------------------------------------------------
    16: }
    17: model {
    18:   mu ~ normall(0, 10);
                             ^
    19:   tau ~ cauchy(0, 10);
  -------------------------------------------------"
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_error_and_info_undefined_function.stan"
              :buffer 'buffer-object))))
      (expect
       (length flycheck-errors)
       :to-equal
       (length correct-errors))
      (expect
       flycheck-errors
       :to-be-equal-flycheck-errors
       correct-errors)))
  ;;
  (it "captures an undefined include file correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_error_undefined_include_file.stancout.txt"
                            test-flycheck-stan-dir)))
           ;; list of flycheck-style error objects
           (flycheck-errors (flycheck-stan-parser
                             error-message 'stanc 'buffer-object))
           ;; Gold standard object
           ;; (flycheck-error-new-at
           ;;  LINE COLUMN &optional LEVEL MESSAGE &key CHECKER ID GROUP
           ;;  (FILENAME (buffer-file-name)) (BUFFER (current-buffer)))
           (correct-errors
            (list (flycheck-error-new-at
                   ;; line column level
                   0 nil 'error
                   ;; message
                   "Error: could not find include file no_such_file.stan in the following directories:"
                   :checker 'stanc
                   :id nil
                   :group nil
                   :filename
                   "examples/example_error_undefined_include_file.stan"
                   :buffer 'buffer-object))))
      (expect
       (length flycheck-errors)
       :to-equal
       (length correct-errors))
      (expect
       flycheck-errors
       :to-be-equal-flycheck-errors
       correct-errors)))
  ;;
  (it "captures an undefined include file correctly even with infos"
    ;; No infos because #include resolution happens first.
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_error_and_info_undefined_include_file.stancout.txt"
                            test-flycheck-stan-dir)))
           ;; list of flycheck-style error objects
           (flycheck-errors (flycheck-stan-parser
                             error-message 'stanc 'buffer-object))
           ;; Gold standard object
           ;; (flycheck-error-new-at
           ;;  LINE COLUMN &optional LEVEL MESSAGE &key CHECKER ID GROUP
           ;;  (FILENAME (buffer-file-name)) (BUFFER (current-buffer)))
           (correct-errors
            (list
             ;; Note #include resolution and failure happen
             ;; before # comment checks.
             (flycheck-error-new-at
              ;; line column level
              0 nil 'error
              ;; message
              "Error: could not find include file no_such_file.stan in the following directories:"
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_error_and_info_undefined_include_file.stan"
              :buffer 'buffer-object))))
      (expect
       (length flycheck-errors)
       :to-equal
       (length correct-errors))
      (expect
       flycheck-errors
       :to-be-equal-flycheck-errors
       correct-errors)))
  ;;
  (it "captures an undefined variable correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_error_undefined_variable.stancout.txt"
                            test-flycheck-stan-dir)))
           ;; list of flycheck-style error objects
           (flycheck-errors (flycheck-stan-parser
                             error-message 'stanc 'buffer-object))
           ;; Gold standard object
           ;; (flycheck-error-new-at
           ;;  LINE COLUMN &optional LEVEL MESSAGE &key CHECKER ID GROUP
           ;;  (FILENAME (buffer-file-name)) (BUFFER (current-buffer)))
           (correct-errors
            (list (flycheck-error-new-at
                   ;; line column level
                   15 12 'error
                   ;; message
                   "Error: Variable \"mu\" does not exist.
 error in 'examples/example_error_undefined_variable.stan' at line 15, column 12
  -------------------------------------------------
    13: transformed parameters {
    14:   vector[J] theta;
    15:   theta = mu + tau * eta;
                   ^
    16: }
  -------------------------------------------------"
                   :checker 'stanc
                   :id nil
                   :group nil
                   :filename
                   "examples/example_error_undefined_variable.stan"
                   :buffer 'buffer-object))))
      (expect
       (length flycheck-errors)
       :to-equal
       (length correct-errors))
      (expect
       flycheck-errors
       :to-be-equal-flycheck-errors
       correct-errors)))
  ;;
  (it "captures an undefined variable correctly even with infos"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_error_and_info_undefined_variable.stancout.txt"
                            test-flycheck-stan-dir)))
           ;; list of flycheck-style error objects
           (flycheck-errors (flycheck-stan-parser
                             error-message 'stanc 'buffer-object))
           ;; Gold standard object
           ;; (flycheck-error-new-at
           ;;  LINE COLUMN &optional LEVEL MESSAGE &key CHECKER ID GROUP
           ;;  (FILENAME (buffer-file-name)) (BUFFER (current-buffer)))
           (correct-errors
            (list (flycheck-error-new-at
                   ;; line column level
                   0 nil 'info
                   ;; message
                   "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
                   :checker 'stanc
                   :id nil
                   :group nil
                   :filename
                   "examples/example_error_and_info_undefined_variable.stan"
                   :buffer 'buffer-object)
                  (flycheck-error-new-at
                   ;; line column level
                   0 nil 'info
                   ;; message
                   "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
                   :checker 'stanc
                   :id nil
                   :group nil
                   :filename
                   "examples/example_error_and_info_undefined_variable.stan"
                   :buffer 'buffer-object)
                  (flycheck-error-new-at
                   ;; line column level
                   0 nil 'info
                   ;; message
                   "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
                   :checker 'stanc
                   :id nil
                   :group nil
                   :filename
                   "examples/example_error_and_info_undefined_variable.stan"
                   :buffer 'buffer-object)
                  (flycheck-error-new-at
                   ;; line column level
                   0 nil 'info
                   ;; message
                   "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
                   :checker 'stanc
                   :id nil
                   :group nil
                   :filename
                   "examples/example_error_and_info_undefined_variable.stan"
                   :buffer 'buffer-object)
                  (flycheck-error-new-at
                   ;; line column level
                   0 nil 'info
                   ;; message
                   "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
                   :checker 'stanc
                   :id nil
                   :group nil
                   :filename
                   "examples/example_error_and_info_undefined_variable.stan"
                   :buffer 'buffer-object)
                  (flycheck-error-new-at
                   ;; line column level
                   15 12 'error
                   ;; message
                   "Error: Variable \"mu\" does not exist.
 error in 'examples/example_error_and_info_undefined_variable.stan' at line 15, column 12
  -------------------------------------------------
    13: transformed parameters {
    14:   vector[J] theta;
    15:   theta = mu + tau * eta;
                   ^
    16: }
  -------------------------------------------------"
                   :checker 'stanc
                   :id nil
                   :group nil
                   :filename
                   "examples/example_error_and_info_undefined_variable.stan"
                   :buffer 'buffer-object))))
      (expect
       (length flycheck-errors)
       :to-equal
       (length correct-errors))
      (expect
       flycheck-errors
       :to-be-equal-flycheck-errors
       correct-errors)))
  ;;
  (it "captures a misspelled block correctly"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_failure_misspelled_block.stancout.txt"
                            test-flycheck-stan-dir)))
           ;; list of flycheck-style error objects
           (flycheck-errors (flycheck-stan-parser
                             error-message 'stanc 'buffer-object))
           ;; Gold standard object
           ;; (flycheck-error-new-at
           ;;  LINE COLUMN &optional LEVEL MESSAGE &key CHECKER ID GROUP
           ;;  (FILENAME (buffer-file-name)) (BUFFER (current-buffer)))
           (correct-errors
            (list (flycheck-error-new-at
                   ;; line column level
                   8 nil 'error
                   ;; message
                   "Error: PARSER FAILED TO PARSE INPUT COMPLETELY
STOPPED AT LINE 8:
pparameters {
  real mu;
  real<lower=0> tau;
  vector[J] eta;
}
transformed parameters {
  vector[J] theta;
  theta = mu + tau * eta;
}
model {
  mu ~ normal(0, 10);
  tau ~ cauchy(0, 10);
  eta ~ normal(0, 1); // implies theta ~ normal(mu, tau)
  y ~ normal(theta, sigma);
}"
                   :checker 'stanc
                   :id nil
                   :group nil
                   :filename
                   "examples/example_failure_misspelled_block.stan"
                   :buffer 'buffer-object))))
      (expect
       (length flycheck-errors)
       :to-equal
       (length correct-errors))
      (expect
       flycheck-errors
       :to-be-equal-flycheck-errors
       correct-errors)))
  ;;
  (it "captures a misspelled block correctly even with infos"
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/example_failure_and_info_misspelled_block.stancout.txt"
                            test-flycheck-stan-dir)))
           ;; list of flycheck-style error objects
           (flycheck-errors (flycheck-stan-parser
                             error-message 'stanc 'buffer-object))
           ;; Gold standard object
           ;; (flycheck-error-new-at
           ;;  LINE COLUMN &optional LEVEL MESSAGE &key CHECKER ID GROUP
           ;;  (FILENAME (buffer-file-name)) (BUFFER (current-buffer)))
           (correct-errors
            (list
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_failure_and_info_misspelled_block.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_failure_and_info_misspelled_block.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_failure_and_info_misspelled_block.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_failure_and_info_misspelled_block.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              8 nil 'error
              ;; message
              "Error: PARSER FAILED TO PARSE INPUT COMPLETELY
STOPPED AT LINE 8:
pparameters {
  real mu;
  real<lower=0> tau;
  vector[J] eta;
}
transformed parameters {
  vector[J] theta;
  theta = mu + tau * eta;
}
model {
  mu ~ normal(0, 10);
  tau ~ cauchy(0, 10);
  eta ~ normal(0, 1); # implies theta ~ normal(mu, tau)
  y ~ normal(theta, sigma);
}"
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_failure_and_info_misspelled_block.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_failure_and_info_misspelled_block.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_failure_and_info_misspelled_block.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_failure_and_info_misspelled_block.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'info
              ;; message
              "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
              :checker 'stanc
              :id nil
              :group nil
              :filename
              "examples/example_failure_and_info_misspelled_block.stan"
              :buffer 'buffer-object))))
      (expect
       (length flycheck-errors)
       :to-equal
       (length correct-errors))
      (expect
       flycheck-errors
       :to-be-equal-flycheck-errors
       correct-errors)))
  ;;
  (it "handles an error without header correctly (number starting the file name)"
    ;; Spying
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-File-Name.html
    ;; (buffer-file-name (current-buffer)) needs to return a fake file name.
    ;; https://github.com/jorgenschaefer/emacs-buttercup/blob/master/docs/writing-tests.md#spies-and-return-value
    ;; https://github.com/jorgenschaefer/emacs-buttercup/issues/122
    (spy-on 'buffer-file-name
            :and-return-value
            "examples/01_example_error_number_model_name.stan")
    ;;
    (let* ((error-message (test-flycheck-stan--get-string-from-file
                           (expand-file-name
                            "./examples/01_example_error_number_model_name.stancout.txt"
                            test-flycheck-stan-dir)))
           ;; list of flycheck-style error objects
           (flycheck-errors (flycheck-stan-parser
                             error-message 'stanc 'buffer-object))
           ;; Gold standard object
           ;; (flycheck-error-new-at
           ;;  LINE COLUMN &optional LEVEL MESSAGE &key CHECKER ID GROUP
           ;;  (FILENAME (buffer-file-name)) (BUFFER (current-buffer)))
           (correct-errors
            (list
             (flycheck-error-new-at
              ;; line column level
              0 nil 'error
              ;; message
              "Error: model_name must not start with a number or symbol other than _"
              :checker 'stanc
              :id nil
              :group 'other_error
              :filename
              "examples/01_example_error_number_model_name.stan"
              :buffer 'buffer-object)
             (flycheck-error-new-at
              ;; line column level
              0 nil 'error
              ;; message
              "Error: Could not remove output file=examples/01_example_error_number_model_name.cpp"
              :checker 'stanc
              :id nil
              :group 'other_error
              :filename
              "examples/01_example_error_number_model_name.stan"
              :buffer 'buffer-object))))
      (expect
       (length flycheck-errors)
       :to-equal
       (length correct-errors))
      ;; Given the same length this comparison is possible.
      ;; This tends to be more informative than
      ;; :to-be-equal-flycheck-errors.
      (cl-mapcar (lambda (a b)
                   (expect a :to-equal b))
                 flycheck-errors
                 correct-errors)
      (expect
       flycheck-errors
       :to-be-equal-flycheck-errors
       correct-errors))))


;;;
;;; Tests that require the `stanc' binary.
;; Nice example of `before-all', `before-each', `assume', etc.
;; https://github.com/flycheck/flycheck/blob/master/test/specs/test-melpa-package.el
;; Examples of testing flycheck checkers.
;; https://github.com/search?q=flycheck-current-errors+&type=Code

;; Helper functions from an old version of `flycheck'.
;; These are not currently used for simplicity.
;; https://github.com/daniel-ness/.emacs.d/blob/695fc3e0c2ff8476879e416e83e528a2794a1946/manual/flycheck-master/test/test-helper.el

(defvar test-flycheck-stan-sleep-for-time 1.0
  "Time in seconds to sleep for while waiting for async stanc process.
This may need adaptation depending on the system used for testing.")

(describe "stanc checker working live (check test-flycheck-stan-sleep-for-time first if errors)"
  (let* ((stanc-path (executable-find "stanc")))
    ;; No tests will be run if stanc is not found.
    (assume stanc-path)
    ;;
    (it "detects issues same as static gold standard (error_and_info_composite)"
      ;; Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
      ;; https://emacs.stackexchange.com/questions/33915/problem-with-save-current-buffer-and-find-file
      (with-current-buffer (find-file-noselect
                            (expand-file-name
                             "examples/example_error_and_info_composite.stan"
                             test-flycheck-stan-dir))
        (read-only-mode +1)
        (stan-mode)
        (flycheck-stan-setup)
        (flycheck-mode)
        (flycheck-buffer)
        ;; Needs to wait for the async process.
        (sleep-for test-flycheck-stan-sleep-for-time)
        ;; Gold standard
        ;; It is manually specified rather than parsed from the error file to
        ;; manipulate the file name.
        (let* ((stan-file-name (expand-file-name
                                "examples/example_error_and_info_composite.stan"
                                test-flycheck-stan-dir))
               (stan-buffer (current-buffer))
               (flycheck-correct-errors
                (list
                 (flycheck-error-new-at
                  ;; line column level
                  0 nil 'info
                  ;; message
                  "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
                  :checker 'stanc
                  :id nil
                  :group nil
                  :filename stan-file-name
                  :buffer stan-buffer)
                 (flycheck-error-new-at
                  ;; line column level
                  0 nil 'info
                  ;; message
                  "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
                  :checker 'stanc
                  :id nil
                  :group nil
                  :filename stan-file-name
                  :buffer stan-buffer)
                 (flycheck-error-new-at
                  ;; line column level
                  0 nil 'info
                  ;; message
                  "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
                  :checker 'stanc
                  :id nil
                  :group nil
                  :filename stan-file-name
                  :buffer stan-buffer)
                 (flycheck-error-new-at
                  ;; line column level
                  0 nil 'info
                  ;; message
                  "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
                  :checker 'stanc
                  :id nil
                  :group nil
                  :filename stan-file-name
                  :buffer stan-buffer)
                 (flycheck-error-new-at
                  ;; line column level
                  9 2 'error
                  ;; message
                  (concat "Error:
 error in '" stan-file-name "' at line 9, column 2
  -------------------------------------------------
     7: }
     8: parameters {
     9:   rear mu;
         ^
    10:   // The parser stops at the above line.
  -------------------------------------------------
PARSER EXPECTED: <one of the following:
  a variable declaration, beginning with type,
      (int, real, vector, row_vector, matrix, unit_vector,
       simplex, ordered, positive_ordered,
       corr_matrix, cov_matrix,
       cholesky_corr, cholesky_cov
  or '}' to close variable declarations>")
                  :checker 'stanc
                  :id nil
                  :group nil
                  :filename stan-file-name
                  :buffer stan-buffer))))
          ;;
          (expect
           (length flycheck-current-errors)
           :to-equal
           (length flycheck-correct-errors))
          ;;
          (expect
           (seq-map #'flycheck-error-line
                    flycheck-current-errors)
           :to-equal
           (seq-map #'flycheck-error-line
                    flycheck-correct-errors))
          ;;
          (expect
           (seq-map #'flycheck-error-column
                    flycheck-current-errors)
           :to-equal
           (seq-map #'flycheck-error-column
                    flycheck-correct-errors))
          ;;
          ;; Given the same length this comparison is possible.
          ;; This tends to be more informative than
          ;; :to-be-equal-flycheck-errors.
          (cl-mapcar (lambda (a b)
                       (expect a :to-equal b))
                     flycheck-current-errors
                     flycheck-correct-errors)
          ;;
          (expect
           flycheck-current-errors
           :to-be-equal-flycheck-errors
           flycheck-correct-errors))))
    ;;
    (it "detects issues same as static gold standard (info_composite_with_error)"
      ;; Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
      ;; https://emacs.stackexchange.com/questions/33915/problem-with-save-current-buffer-and-find-file
      (with-current-buffer (find-file-noselect
                            (expand-file-name
                             "examples/example_info_composite_with_error.stan"
                             test-flycheck-stan-dir))
        (read-only-mode +1)
        (stan-mode)
        (flycheck-stan-setup)
        (flycheck-mode)
        (flycheck-buffer)
        ;; Needs to wait for the async process.
        (sleep-for test-flycheck-stan-sleep-for-time)
        ;; Gold standard
        ;; It is manually specified rather than parsed from the error file to
        ;; manipulate the file name.
        (let* ((stan-file-name (expand-file-name
                                "examples/example_info_composite_with_error.stan"
                                test-flycheck-stan-dir))
               (stan-buffer (current-buffer))
               (flycheck-correct-errors
                (list
                 (flycheck-error-new-at
                  ;; line column level
                  0 nil 'info
                  ;; message
                  "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
                  :checker 'stanc
                  :id nil
                  :group nil
                  :filename stan-file-name
                  :buffer stan-buffer)
                 (flycheck-error-new-at
                  ;; line column level
                  0 nil 'info
                  ;; message
                  "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
                  :checker 'stanc
                  :id nil
                  :group nil
                  :filename stan-file-name
                  :buffer stan-buffer)
                 (flycheck-error-new-at
                  ;; line column level
                  0 nil 'info
                  ;; message
                  "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
                  :checker 'stanc
                  :id nil
                  :group nil
                  :filename stan-file-name
                  :buffer stan-buffer)
                 (flycheck-error-new-at
                  ;; line column level
                  0 nil 'info
                  ;; message
                  "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
                  :checker 'stanc
                  :id nil
                  :group nil
                  :filename stan-file-name
                  :buffer stan-buffer)
                 (flycheck-error-new-at
                  ;; line column level
                  0 nil 'info
                  ;; message
                  "Info: assignment operator <- deprecated in the Stan language; use = instead."
                  :checker 'stanc
                  :id nil
                  :group nil
                  :filename stan-file-name
                  :buffer stan-buffer)
                 (flycheck-error-new-at
                  ;; line column level
                  0 nil 'info
                  ;; message
                  "Info:
Left-hand side of sampling statement (~) may contain a non-linear transform of a parameter or local variable.
If it does, you need to include a target += statement with the log absolute determinant of the Jacobian of the transform.
Left-hand-side of sampling statement:
    stan::math::exp(stan::math::log(mu)) ~ normal(...)"
                  :checker 'stanc
                  :id nil
                  :group nil
                  :filename stan-file-name
                  :buffer stan-buffer)
                 (flycheck-error-new-at
                  ;; line column level
                  0 nil 'info
                  ;; message
                  "Info: increment_log_prob(...); is deprecated and will be removed in the future.
  Use target += ...; instead."
                  :checker 'stanc
                  :id nil
                  :group nil
                  :filename stan-file-name
                  :buffer stan-buffer)
                 (flycheck-error-new-at
                  ;; line column level
                  0 nil 'info
                  ;; message
                  "Info: Deprecated function 'cauchy_log'; please replace suffix '_log' with '_lpdf' for density functions or '_lpmf' for mass functions"
                  :checker 'stanc
                  :id nil
                  :group nil
                  :filename stan-file-name
                  :buffer stan-buffer)
                 (flycheck-error-new-at
                  ;; line column level
                  0 nil 'info
                  ;; message
                  "Info: Comments beginning with # are deprecated.  Please use // in place of # for line comments."
                  :checker 'stanc
                  :id nil
                  :group nil
                  :filename stan-file-name
                  :buffer stan-buffer)
                 (flycheck-error-new-at
                  ;; line column level
                  0 nil 'info
                  ;; message
                  "Info: increment_log_prob(...); is deprecated and will be removed in the future.
  Use target += ...; instead."
                  :checker 'stanc
                  :id nil
                  :group nil
                  :filename stan-file-name
                  :buffer stan-buffer)
                 (flycheck-error-new-at
                  ;; line column level
                  21 34 'error
                  ;; message
                  (concat "Error: Probabilty functions with suffixes _lpdf, _lpmf, _lcdf, and _lccdf,
require a vertical bar (|) between the first two arguments.
 error in '" stan-file-name "' at line 21, column 34
  -------------------------------------------------
    19:   increment_log_prob(cauchy_log(tau, 0, 10));
    20:   eta ~ normal(0, 1); # implies theta ~ normal(mu, tau)
    21:   increment_log_prob(normal_lpdf(y, theta, sigma));
                                         ^
    22: }
  -------------------------------------------------
PARSER EXPECTED: \"|\"")
                  :checker 'stanc
                  :id nil
                  :group nil
                  :filename stan-file-name
                  :buffer stan-buffer))))
          ;;
          (expect
           (length flycheck-current-errors)
           :to-equal
           (length flycheck-correct-errors))
          ;;
          (expect
           (seq-map #'flycheck-error-line
                    flycheck-current-errors)
           :to-equal
           (seq-map #'flycheck-error-line
                    flycheck-correct-errors))
          ;;
          (expect
           (seq-map #'flycheck-error-column
                    flycheck-current-errors)
           :to-equal
           (seq-map #'flycheck-error-column
                    flycheck-correct-errors))
          ;;
          ;; Given the same length this comparison is possible.
          ;; This tends to be more informative than
          ;; :to-be-equal-flycheck-errors.
          (cl-mapcar (lambda (a b)
                       (expect a :to-equal b))
                     flycheck-current-errors
                     flycheck-correct-errors)
          ;;
          (expect
           flycheck-current-errors
           :to-be-equal-flycheck-errors
           flycheck-correct-errors))))
    ;;
    (it "handles a bad file name correctly (number starting the file name)"
      ;; This error message does not have a header with file name, so the file
      ;; name is taken from the buffer-file-name.
      ;;
      ;; Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
      ;; https://emacs.stackexchange.com/questions/33915/problem-with-save-current-buffer-and-find-file
      (with-current-buffer (find-file-noselect
                            (expand-file-name
                             "examples/01_example_error_number_model_name.stan"
                             test-flycheck-stan-dir))
        (read-only-mode +1)
        (stan-mode)
        (flycheck-stan-setup)
        (flycheck-mode)
        (flycheck-buffer)
        ;; Needs to wait for the async process.
        (sleep-for test-flycheck-stan-sleep-for-time)
        ;; Gold standard
        ;; Define here rather than use the stancout.txt file.
        ;; The filename (absolute path) and buffer object can be tested.
        (let* ((stan-file-name (concat
                                test-flycheck-stan-dir
                                "examples/01_example_error_number_model_name.stan"))
               (stan-buffer (current-buffer))
               (flycheck-correct-errors
                (list
                 (flycheck-error-new-at
                  ;; line column level
                  0 nil 'error
                  ;; message
                  "Error: model_name must not start with a number or symbol other than _"
                  :checker 'stanc
                  :id nil
                  :group 'other_error
                  ;; Here the :filename should be full path for a live test
                  ;; that runs stanc live.
                  :filename stan-file-name
                  ;; We can use (current-buffer) within `temp-buffer'.
                  :buffer stan-buffer)
                 (flycheck-error-new-at
                  ;; line column level
                  0 nil 'error
                  ;; message
                  ;; Note that somehow _model is added to the name by stanc.
                  "Error: Could not remove output file=01_example_error_number_model_name_model.cpp"
                  :checker 'stanc
                  :id nil
                  :group 'other_error
                  :filename stan-file-name
                  ;; We can use (current-buffer) within `temp-buffer'.
                  :buffer stan-buffer))))
          ;;
          (expect
           (length flycheck-current-errors)
           :to-equal
           (length flycheck-correct-errors))
          ;;
          (expect
           (seq-map #'flycheck-error-line
                    flycheck-current-errors)
           :to-equal
           (seq-map #'flycheck-error-line
                    flycheck-correct-errors))
          ;;
          (expect
           (seq-map #'flycheck-error-column
                    flycheck-current-errors)
           :to-equal
           (seq-map #'flycheck-error-column
                    flycheck-correct-errors))
          ;;
          ;; Given the same length this comparison is possible.
          ;; This tends to be more informative than
          ;; :to-be-equal-flycheck-errors.
          (cl-mapcar (lambda (a b)
                       (expect a :to-equal b))
                     flycheck-current-errors
                     flycheck-correct-errors)
          ;;
          (expect
           flycheck-current-errors
           :to-be-equal-flycheck-errors
           flycheck-correct-errors))))))


(provide 'test-flycheck-stan)
;;;
;;; test-flycheck-stan.el ends here
