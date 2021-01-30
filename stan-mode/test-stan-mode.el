;;; test-stan-mode.el --- A buttercup test suite for stan-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kazuki Yoshida

;; Author: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; URL: https://github.com/stan-dev/stan-mode/tree/master/stan-mode
;; Keywords: languages
;; Version: 10.1.0
;; Created: 2019-07-26
;; Package-Requires: ((emacs "25.1"))

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
;;
;; References:
;;
;; Behavior-Driven Emacs Lisp Testing
;;  https://github.com/jorgenschaefer/emacs-buttercup
;; Writing Tests
;;  https://github.com/jorgenschaefer/emacs-buttercup/blob/master/docs/writing-tests.md
;;
;; Emacs Stack Exchange: how can I unit test my font-face rules?
;;  https://emacs.stackexchange.com/questions/45775/how-can-i-unit-test-my-font-face-rules
;; groovy-unit-test.el (tests for indentation and font-locks)
;;  https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/tree/master/test

;;

;;; Code:
(require 'buttercup)
(require 'shut-up)
(require 'seq)
(require 'rx)
(require 'stan-mode)
(require 'stan-keywords)

;; This hook must be added to configure `stan-mode' correctly.
(add-hook 'stan-mode-hook 'stan-mode-setup)

;; Record the directory of this file up front.
;; This does not work within `it'.
(defvar test-stan-mode-dir (file-name-directory
                            (or load-file-name buffer-file-name)))

;;; Overall
;; Defining Derived Modes
;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Derived-Modes.html
;; Major Mode Conventions
;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Major-Mode-Conventions.html
(describe "stan-mode"
  (it "can activate as a major mode on a temp-buffer"
    (expect
     (with-temp-buffer
       (stan-mode))
     :to-equal nil))
  ;;
  (it "is derived from the prog-mode"
    (expect
     (with-temp-buffer
       (stan-mode)
       (derived-mode-p 'prog-mode))
     :to-be-truthy)))


;;; Syntax Table
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Tables.html
;; Syntax Descriptors
;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Descriptors.html
;; Syntax Table Functions
;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Table-Functions.html
;;
;; Define these to avoid byte compiler warnings.
;; These top-level symbols were used as the `let' form did not work well.
;; TODO: More buttercup-like solutions.
(defvar stan-mode-result)
(defvar c++-mode-result)
(defvar stan-mode-result-for-bar)
(defvar stan-mode-result-for-comma)
;;
(describe "stan-mode-syntax-table"
  (it "is a valid syntax table"
    (expect
     (syntax-table-p stan-mode-syntax-table)
     :to-be-truthy))
  ;;
  (it "treats // as in c++-mode"
    (expect
     (with-temp-buffer
       (stan-mode)
       (char-syntax ?/))
     :to-equal
     (with-temp-buffer
       (c++-mode)
       (char-syntax ?/)))
    ;;
    (with-temp-buffer
      ;; code from rstanarm
      (insert
       "model {
  if (prior_PD == 0) for (j in 1:J) { // likelihood contribution for each group
    real shift = dot_product(xbarR_inv[j], theta[j]);
    real dummy = ll_mvn_ols_qr_lp(theta[j], Rb[j],
                                  has_intercept == 1 ? alpha[j] + shift : shift,
                                  ybar[j], SSR[j], sigma[j], N[j]);
    // implicit: u[j] is uniform on the surface of a hypersphere
  }
  if (has_intercept == 1 && prior_dist_for_intercept > 0)
    target += normal_lpdf(z_alpha | 0, 1);
  if (prior_dist == 1) {
    if (K > 1) target += beta_lpdf( R2  | half_K, eta);
    else target += beta_lpdf(square(R2) | half_K, eta) + sum(log(fabs(R2)));
  }
  // implicit: log_omega is uniform over the real line for all j
}")
      (shut-up (stan-mode)
               (goto-char (point-min))
               (search-forward "// likelihood")
               (search-backward "// likelihood")
               (setq stan-mode-result (syntax-after (point)))
               (c++-mode)
               (goto-char (point-min))
               (search-forward "// likelihood")
               (search-backward "// likelihood")
               (setq c++-mode-result (syntax-after (point)))
               ;; Return this
               (expect
                stan-mode-result
                :to-equal
                c++-mode-result))
      ;; second /
      (shut-up (stan-mode)
               (goto-char (point-min))
               (search-forward "/ likelihood")
               (search-backward "/ likelihood")
               (setq stan-mode-result (syntax-after (point)))
               (c++-mode)
               (goto-char (point-min))
               (search-forward "/ likelihood")
               (search-backward "/ likelihood")
               (setq c++-mode-result (syntax-after (point)))
               ;; Return this
               (expect
                stan-mode-result
                :to-equal
                c++-mode-result))
      ;;
      (shut-up (stan-mode)
               (goto-char (point-min))
               (search-forward "// implicit: u")
               (search-backward "// implicit: u")
               (setq stan-mode-result (syntax-after (point)))
               (c++-mode)
               (goto-char (point-min))
               (search-forward "// implicit: u")
               (search-backward "// implicit: u")
               (setq c++-mode-result (syntax-after (point)))
               ;; Return this
               (expect
                stan-mode-result
                :to-equal
                c++-mode-result))
      ;; second /
      (shut-up (stan-mode)
               (goto-char (point-min))
               (search-forward "/ implicit: u")
               (search-backward "/ implicit: u")
               (setq stan-mode-result (syntax-after (point)))
               (c++-mode)
               (goto-char (point-min))
               (search-forward "/ implicit: u")
               (search-backward "/ implicit: u")
               (setq c++-mode-result (syntax-after (point)))
               ;; Return this
               (expect
                stan-mode-result
                :to-equal
                c++-mode-result))))
  ;;
  (it "treats a single | as in normal_pdf(y | mu, sigma) as a deliminator"
    (expect
     (with-temp-buffer
       (stan-mode)
       (char-syntax ?|))
     :to-equal ?.)
    ;;
    (with-temp-buffer
      ;; code from rstanarm
      (insert
       "model {
  if (prior_PD == 0) for (j in 1:J) { // likelihood contribution for each group
    real shift = dot_product(xbarR_inv[j], theta[j]);
    real dummy = ll_mvn_ols_qr_lp(theta[j], Rb[j],
                                  has_intercept == 1 ? alpha[j] + shift : shift,
                                  ybar[j], SSR[j], sigma[j], N[j]);
    // implicit: u[j] is uniform on the surface of a hypersphere
  }
  if (has_intercept == 1 && prior_dist_for_intercept > 0)
    target += normal_lpdf(z_alpha | 0, 1);
  if (prior_dist == 1) {
    if (K > 1) target += beta_lpdf( R2  | half_K, eta);
    else target += beta_lpdf(square(R2) | half_K, eta) + sum(log(fabs(R2)));
  }
  // implicit: log_omega is uniform over the real line for all j
}")
      (shut-up (stan-mode)
               (goto-char (point-min))
               (search-forward "| 0, 1")
               (search-backward "| 0, 1")
               (setq stan-mode-result-for-bar (syntax-after (point)))
               (search-forward ", 1")
               (search-backward ", 1")
               (setq stan-mode-result-for-comma (syntax-after (point)))
               ;; Return this
               (expect
                stan-mode-result-for-bar
                :to-equal
                stan-mode-result-for-comma))
      ;;
      (shut-up (stan-mode)
               (goto-char (point-min))
               (search-forward "| half_K, eta")
               (search-backward "| half_K, eta")
               (setq stan-mode-result-for-bar (syntax-after (point)))
               (search-forward ", eta")
               (search-backward ", eta")
               (setq stan-mode-result-for-comma (syntax-after (point)))
               ;; Return this
               (expect
                stan-mode-result-for-bar
                :to-equal
                stan-mode-result-for-comma))))
  ;;
  (it "treats # differently from c++-mode"
    (expect
     (with-temp-buffer
       (stan-mode)
       (char-syntax ?#))
     :not :to-equal
     (with-temp-buffer
       (c++-mode)
       (char-syntax ?#)))
    (with-temp-buffer
      ;; insert code from rstanarm
      (insert
       "#include /pre/Columbia_copyright.stan
#include /pre/license.stan

// GLM for a Bernoulli outcome
functions {
#include /functions/common_functions.stan
#include /functions/bernoulli_likelihoods.stan
}
")
      (shut-up (stan-mode)
               (goto-char (point-min))
               (search-forward "#include /pre")
               (search-backward "#include /pre")
               (setq stan-mode-result (syntax-after (point)))
               ;; Change to c++-mode
               (c++-mode)
               (goto-char (point-min))
               (search-forward "#include /pre")
               (search-backward "#include /pre")
               (setq c++-mode-result (syntax-after (point)))
               ;; Return this
               (expect
                stan-mode-result
                :not :to-equal
                c++-mode-result))
      ;;
      (shut-up (stan-mode)
               (goto-char (point-min))
               (search-forward "#include /functions")
               (search-backward "#include /functions")
               (setq stan-mode-result (syntax-after (point)))
               ;; Change to c++-mode
               (c++-mode)
               (goto-char (point-min))
               (search-forward "#include /functions")
               (search-backward "#include /functions")
               (setq c++-mode-result (syntax-after (point)))
               ;; Return this
               (expect
                stan-mode-result
                :not :to-equal
                c++-mode-result))))
  ;;
  (xit "handles paired <...> (parentheses) like c++-mode (4 . 62) and (5 . 60)"
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Table-Internals.html
    (with-temp-buffer
      ;; insert modified fake code from rstanarm
      (insert
       "data {
  // more glmer stuff
  int<lower=0> num_non_zero[2];     // number of non-zero elements in the Z matrices
  vector[num_non_zero[1]] w0;       // non-zero elements in the implicit Z0 matrix
  vector[num_non_zero[2]] w1;       // non-zero elements in the implicit Z1 matrix
  int<lower=0, upper = q - 1> v0[num_non_zero[1]]; // column indices for w0
  int<lower=0, upper = q - 1> v1[num_non_zero[2]]; // column indices for w1
  // where the non-zeros start in each row of Z0
  int<lower=0, upper = rows(w0) + 1> u0[t > 0 ? N[1] + 1 : 0];
  // where the non-zeros start in each row of Z1
  int<lower=0, upper = rows(w1) + 1> u1[t < 0 ? N[2] + 1 : 0];
  int<lower=0, upper=1> special_case;     // whether we only have to deal with (1|group)
}")
      (shut-up (stan-mode)
               (goto-char (point-min))
               (search-forward "<lower=0")
               (search-backward "<lower=0")
               (setq stan-mode-result (syntax-after (point)))
               ;; Change to c++-mode
               (c++-mode)
               (goto-char (point-min))
               (search-forward "<lower=0")
               (search-backward "<lower=0")
               (setq c++-mode-result (syntax-after (point)))
               ;; Return this
               (expect
                stan-mode-result
                :to-equal
                c++-mode-result))
      ;;
      (shut-up (stan-mode)
               (goto-char (point-min))
               (search-forward "upper = q - 1>")
               (search-backward ">")
               (setq stan-mode-result (syntax-after (point)))
               ;; Change to c++-mode
               (c++-mode)
               (goto-char (point-min))
               (search-forward "upper = q - 1>")
               (search-backward ">")
               (setq c++-mode-result (syntax-after (point)))
               ;; Return this
               (expect
                stan-mode-result
                :to-equal
                c++-mode-result))))
  ;;
  (it "handles isolated < and > (inequalities) like c++-mode (1)"
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Table-Internals.html
    (with-temp-buffer
      ;; insert modified fake code from rstanarm
      (insert
       "data {
  // more glmer stuff
  int<lower=0> num_non_zero[2];     // number of non-zero elements in the Z matrices
  vector[num_non_zero[1]] w0;       // non-zero elements in the implicit Z0 matrix
  vector[num_non_zero[2]] w1;       // non-zero elements in the implicit Z1 matrix
  int<lower=0, upper = q - 1> v0[num_non_zero[1]]; // column indices for w0
  int<lower=0, upper = q - 1> v1[num_non_zero[2]]; // column indices for w1
  // where the non-zeros start in each row of Z0
  int<lower=0, upper = rows(w0) + 1> u0[t > 0 ? N[1] + 1 : 0];
  // where the non-zeros start in each row of Z1
  int<lower=0, upper = rows(w1) + 1> u1[t < 0 ? N[2] + 1 : 0];
  int<lower=0, upper=1> special_case;     // whether we only have to deal with (1|group)
}")
      (shut-up (stan-mode)
               (goto-char (point-min))
               (search-forward "u0[t > 0")
               (search-backward ">")
               (setq stan-mode-result (syntax-after (point)))
               ;; Change to c++-mode
               (c++-mode)
               (goto-char (point-min))
               (search-forward "u0[t > 0")
               (search-backward ">")
               (setq c++-mode-result (syntax-after (point)))
               ;; Return this
               (expect
                stan-mode-result
                :to-equal
                c++-mode-result))
      ;;
      (shut-up (stan-mode)
               (goto-char (point-min))
               (search-forward "u1[t < 0")
               (search-backward "<")
               (setq stan-mode-result (syntax-after (point)))
               ;; Change to c++-mode
               (c++-mode)
               (goto-char (point-min))
               (search-forward "u1[t < 0")
               (search-backward "<")
               (setq c++-mode-result (syntax-after (point)))
               ;; Return this
               (expect
                stan-mode-result
                :to-equal
                c++-mode-result)))))


;;; Indentation
;; groovy-unit-test.el (tests for indentation and font-locks)
;;  https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/tree/master/test
(defun test-stan--indent-string (source)
  "Indent string assuming it is a stan file content.

Returns indented version of SOURCE."
  (with-temp-buffer
    (insert source)
    (stan-mode)
    (shut-up
      (indent-region (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun test-stan--expect-indent-to (source result)
  "Indent SOURCE and compare to RESULT.

This function contains expect in its body.
Do not use inside expect."
  (expect
   (test-stan--indent-string source)
   :to-equal
   result))

(defun test-stan--expect-preserve-indent (source)
  "Specialized expect to check preserved indentation in SOURCE.

This function contains expect in its body.
Do not use inside expect."
  (expect
   (test-stan--indent-string source)
   :to-equal
   source))

(defun test-stan--get-string-from-file (filepath)
  "Return the contents of FILEPATH as a string.

Adopted from http://ergoemacs.org/emacs/elisp_read_file_content.html"
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))


(describe "stan-mode indentation (simple strings)"
  (it "does not break with // as the first thing in the buffer"
    (test-stan--expect-preserve-indent
     "// comment"))
  ;;
  (it "does not break with function as the first thing in the buffer"
    (test-stan--expect-preserve-indent
     "function {"))
  ;;
  (it "does not break with # as the very first character"
    (test-stan--expect-preserve-indent
     "# "))
  ;;
  (it "does not break with #include as the very first statement"
    (test-stan--expect-preserve-indent
     "#include /pre/Columbia_copyright.stan")))


(describe "stan-mode indentation (Eight Schools Example)"
  ;;
  (it "does not break correctly indented code (default indent 2 SPC)"
    (test-stan--expect-preserve-indent
     "// The Eight Schools example with non-centered parametrization.
// https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html
data {
  int<lower=0> J;
  vector[J] y;
  vector<lower=0>[J] sigma;
}
parameters {
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
}
")
    ;;
    (test-stan--expect-preserve-indent
     "// The Eight Schools example with non-centered parametrization.
// https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html
data {
  int<lower=0> J;
  vector[J] y;
  vector<lower=0>[J] sigma;
}
parameters {
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
}
"))
  ;;
  (it "responds to alternative stan-indentation-offset settings"
    (let ((stan-indentation-offset 3))
      (test-stan--expect-preserve-indent
       "// The Eight Schools example with non-centered parametrization.
// https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html
data {
   int<lower=0> J;
   vector[J] y;
   vector<lower=0>[J] sigma;
}
parameters {
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
}
"))
    ;;
    (let ((stan-indentation-offset 5))
      (test-stan--expect-preserve-indent
       "// The Eight Schools example with non-centered parametrization.
// https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html
data {
     int<lower=0> J;
     vector[J] y;
     vector<lower=0>[J] sigma;
}
parameters {
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
}
"))
    ;;
    (let ((stan-indentation-offset 5))
      (test-stan--expect-indent-to
       ;; This should indent
       "// The Eight Schools example with non-centered parametrization.
// https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html
data {
  int<lower=0> J;
  vector[J] y;
  vector<lower=0>[J] sigma;
}
parameters {
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
}
"
       ;; to this.
       "// The Eight Schools example with non-centered parametrization.
// https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html
data {
     int<lower=0> J;
     vector[J] y;
     vector<lower=0>[J] sigma;
}
parameters {
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
}
"))))


(describe "stan-mode indentation (rstanarm examples)"
  (it "does not indent #include even if it is inside a block (bare implementation)"
    (expect
     (with-temp-buffer
       (insert "// GLM for a Bernoulli outcome
functions {
#include /functions/common_functions.stan
#include /functions/bernoulli_likelihoods.stan
  # deprecated comment should be indented.
}")
       (stan-mode)
       (setq indent-tabs-mode nil)
       (shut-up
         (indent-region (point-min) (point-max)))
       (buffer-substring-no-properties (point-min) (point-max)))
     :to-equal
     "// GLM for a Bernoulli outcome
functions {
#include /functions/common_functions.stan
#include /functions/bernoulli_likelihoods.stan
  # deprecated comment should be indented.
}"))
  ;;
  (it "does not indent #include even if it is inside a block"
    (test-stan--expect-preserve-indent
     "// GLM for a Bernoulli outcome
functions {
#include /functions/common_functions.stan
#include /functions/bernoulli_likelihoods.stan
  # deprecated comment should be indented.
}"))
  ;;
  (it "does not break indentation of bernoulli.stan"
    (test-stan--expect-preserve-indent
     (test-stan--get-string-from-file
      (expand-file-name
       "../rstanarm/src/stan_files/bernoulli.stan"
       test-stan-mode-dir))))
  (it "does not break indentation of binomial.stan"
    (test-stan--expect-preserve-indent
     (test-stan--get-string-from-file
      (expand-file-name
       "../rstanarm/src/stan_files/binomial.stan"
       test-stan-mode-dir))))
  (it "does not break indentation of continuous.stan"
    (test-stan--expect-preserve-indent
     (test-stan--get-string-from-file
      (expand-file-name
       "../rstanarm/src/stan_files/continuous.stan"
       test-stan-mode-dir))))
  (it "does not break indentation of count.stan"
    (test-stan--expect-preserve-indent
     (test-stan--get-string-from-file
      (expand-file-name
       "../rstanarm/src/stan_files/count.stan"
       test-stan-mode-dir))))
  (it "does not break indentation of jm.stan"
    (test-stan--expect-preserve-indent
     (test-stan--get-string-from-file
      (expand-file-name
       "../rstanarm/src/stan_files/jm.stan"
       test-stan-mode-dir))))
  (it "does not break indentation of lm.stan"
    (test-stan--expect-preserve-indent
     (test-stan--get-string-from-file
      (expand-file-name
       "../rstanarm/src/stan_files/lm.stan"
       test-stan-mode-dir))))
  (it "does not break indentation of mvmer.stan"
    (test-stan--expect-preserve-indent
     (test-stan--get-string-from-file
      (expand-file-name
       "../rstanarm/src/stan_files/mvmer.stan"
       test-stan-mode-dir))))
  (it "does not break indentation of polr.stan"
    (test-stan--expect-preserve-indent
     (test-stan--get-string-from-file
      (expand-file-name
       "../rstanarm/src/stan_files/polr.stan"
       test-stan-mode-dir))))
  ;;
  (it "does not break indentation in functions/bernoulli_likelihoods.stan"
    (test-stan--expect-preserve-indent
     (concat
      "functions {\n"
      (test-stan--get-string-from-file
       (expand-file-name
        "../rstanarm/src/stan_files/functions/bernoulli_likelihoods.stan"
        test-stan-mode-dir))
      "}")))
  (it "does not break indentation in functions/binomial_likelihoods.stan"
    (test-stan--expect-preserve-indent
     (concat
      "functions {\n"
      (test-stan--get-string-from-file
       (expand-file-name
        "../rstanarm/src/stan_files/functions/binomial_likelihoods.stan"
        test-stan-mode-dir))
      "}")))
  (it "does not break indentation in functions/common_functions.stan"
    (test-stan--expect-preserve-indent
     (concat
      "functions {\n"
      (test-stan--get-string-from-file
       (expand-file-name
        "../rstanarm/src/stan_files/functions/common_functions.stan"
        test-stan-mode-dir))
      "}")))
  (it "does not break indentation in functions/continuous_likelihoods.stan"
    (test-stan--expect-preserve-indent
     (concat
      "functions {\n"
      (test-stan--get-string-from-file
       (expand-file-name
        "../rstanarm/src/stan_files/functions/continuous_likelihoods.stan"
        test-stan-mode-dir))
      "}")))
  (it "does not break indentation in functions/count_likelihoods.stan"
    (test-stan--expect-preserve-indent
     (concat
      "functions {\n"
      (test-stan--get-string-from-file
       (expand-file-name
        "../rstanarm/src/stan_files/functions/count_likelihoods.stan"
        test-stan-mode-dir))
      "}")))
  (it "does not break indentation in functions/jm_functions.stan"
    (test-stan--expect-preserve-indent
     (concat
      "functions {\n"
      (test-stan--get-string-from-file
       (expand-file-name
        "../rstanarm/src/stan_files/functions/jm_functions.stan"
        test-stan-mode-dir))
      "}")))
  (it "does not break indentation in functions/mvmer_functions.stan"
    (test-stan--expect-preserve-indent
     (concat
      "functions {\n"
      (test-stan--get-string-from-file
       (expand-file-name
        "../rstanarm/src/stan_files/functions/mvmer_functions.stan"
        test-stan-mode-dir))
      "}")))
  ;;
  (it "does not break indentation of # comments when handling them"
    (test-stan--expect-preserve-indent
     "#include /pre/Columbia_copyright.stan
#include /pre/license.stan
# this is a deprecated comment
# data int lower these should not be highlighted
// GLM for a Bernoulli outcome
functions {
#include /functions/common_functions.stan
#include /functions/bernoulli_likelihoods.stan
}
data {
  # dimensions
  int<lower=0> K;        # number of predictors
  int<lower=0> N[2];     # number of observations where y = 0 and y = 1 respectively
  vector[K] xbar;        # vector of column-means of rbind(X0, X1)
  int<lower=0,upper=1> dense_X; # flag for dense vs. sparse
  matrix[N[1],K] X0[dense_X];   # centered (by xbar) predictor matrix | y = 0
  matrix[N[2],K] X1[dense_X];   # centered (by xbar) predictor matrix | y = 1
}")))


;;; Syntax highlighting
(defun test-stan--highlight (src)
  "Syntax hightlight SRC with `stan-mode'.

Adopted from test/groovy-unit-test.el in the `groovy-mode'."
  (with-temp-buffer
    (insert src)
    (goto-char (point-min))
    (stan-mode)
    ;; Ensure we've syntax-highlighted the whole buffer.
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings
        (font-lock-fontify-buffer)))
    (buffer-string)))

(defun test-stan--check-face (regexp src-highlighted &optional start)
  "Return the face of REGEXP in SRC-HIGHLIGHTED.

START is the optional start position of search."
  ;;
  (let ((pos (string-match regexp src-highlighted start)))
    (if pos
        (get-char-property pos
                           'face
                           src-highlighted)
      (error "String not found! Thus, the face is undefined!"))))

(defmacro test-stan--with-highlighted (src &rest body)
  "Insert SRC in a temporary `stan-mode' buffer, highlight, then run BODY.

Adopted from test/groovy-unit-test.el in the `groovy-mode'."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,src)
     (goto-char (point-min))
     ;; Activate `stan-mode', but don't run any hooks. This doesn't
     ;; matter on Travis, but is defensive when running tests in the
     ;; current Emacs instance.
     (delay-mode-hooks (stan-mode))
     ;; Ensure we've syntax-highlighted the whole buffer.
     (if (fboundp 'font-lock-ensure)
         (font-lock-ensure)
       (with-no-warnings
         (font-lock-fontify-buffer)))
     ,@body))

(describe "stan-mode font lock (Eight Schools example)"
  (let* ((src-highlighted
          (test-stan--highlight
           "// The Eight Schools example with non-centered parametrization.
// https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html
data {
  int<lower=0> J;
  vector[J] y;
  vector<lower=0>[J] sigma;
}
parameters {
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
}
")))
    ;;
    (it "gives comments correct faces"
      (expect
       (test-stan--check-face (rx "// The Eight Schools") src-highlighted)
       :to-be
       'font-lock-comment-delimiter-face)
      (expect
       (test-stan--check-face (rx "The Eight Schools") src-highlighted)
       :to-be
       'font-lock-comment-face)
      (expect
       (test-stan--check-face (rx "http") src-highlighted)
       :to-be
       'font-lock-comment-face)
      (expect
       (test-stan--check-face (rx "// implies") src-highlighted)
       :to-be
       'font-lock-comment-delimiter-face)
      (expect
       (test-stan--check-face (rx "implies") src-highlighted)
       :to-be
       'font-lock-comment-face))
    ;;
    (it "gives block names font-lock-keyword-face"
      (expect
       (test-stan--check-face (rx "data {") src-highlighted)
       :to-be
       'font-lock-keyword-face)
      (expect
       (test-stan--check-face (rx "parameters {") src-highlighted)
       :to-be
       'font-lock-keyword-face)
      (expect
       (test-stan--check-face (rx "transformed parameters {") src-highlighted)
       :to-be
       'font-lock-keyword-face)
      (expect
       (test-stan--check-face (rx "model {") src-highlighted)
       :to-be
       'font-lock-keyword-face))
    ;;
    (it "gives type names font-lock-type-face"
      (expect
       (test-stan--check-face (rx "int<lower=0> J;") src-highlighted)
       :to-be
       'font-lock-type-face)
      (expect
       (test-stan--check-face (rx "vector[J] y;") src-highlighted)
       :to-be
       'font-lock-type-face)
      (expect
       (test-stan--check-face (rx "vector<lower=0>[J] sigma;") src-highlighted)
       :to-be
       'font-lock-type-face)
      (expect
       (test-stan--check-face (rx "real mu;") src-highlighted)
       :to-be
       'font-lock-type-face)
      (expect
       (test-stan--check-face (rx "real<lower=0> tau;") src-highlighted)
       :to-be
       'font-lock-type-face))
    ;;
    (it "gives range names font-lock-keyword-face"
      (expect
       (test-stan--check-face (rx "lower=0> J;") src-highlighted)
       :to-be
       'font-lock-keyword-face)
      (expect
       (test-stan--check-face (rx "lower=0>[J] sigma;") src-highlighted)
       :to-be
       'font-lock-keyword-face)
      (expect
       (test-stan--check-face (rx "lower=0> tau;") src-highlighted)
       :to-be
       'font-lock-keyword-face))
    ;;
    (it "gives assignments font-lock-constant-face"
      (expect
       (test-stan--check-face (rx "=0> J;") src-highlighted)
       :to-be
       'font-lock-constant-face)
      (expect
       (test-stan--check-face (rx "= mu + tau") src-highlighted)
       :to-be
       'font-lock-constant-face)
      (expect
       (test-stan--check-face (rx "~ normal(0, 10);") src-highlighted)
       :to-be
       'font-lock-constant-face)
      (expect
       (test-stan--check-face (rx "~ cauchy(0, 10);") src-highlighted)
       :to-be
       'font-lock-constant-face))
    ;;
    (it "gives variables font-lock-variable-name-face"
      (expect
       (test-stan--check-face (rx "J;") src-highlighted)
       :to-be
       'font-lock-variable-name-face)
      (expect
       (test-stan--check-face (rx "y;") src-highlighted)
       :to-be
       'font-lock-variable-name-face)
      (expect
       (test-stan--check-face (rx "sigma;") src-highlighted)
       :to-be
       'font-lock-variable-name-face)
      (expect
       (test-stan--check-face (rx "mu;") src-highlighted)
       :to-be
       'font-lock-variable-name-face)
      (expect
       (test-stan--check-face (rx "tau;") src-highlighted)
       :to-be
       'font-lock-variable-name-face)
      (expect
       (test-stan--check-face (rx "eta;") src-highlighted)
       :to-be
       'font-lock-variable-name-face)
      (expect
       (test-stan--check-face (rx "theta;") src-highlighted)
       :to-be
       'font-lock-variable-name-face))))


(describe "stan-mode font lock (rstanarm bernoulli example)"
  (let* ((src-highlighted
          (test-stan--highlight
           (test-stan--get-string-from-file
            (expand-file-name
             "../rstanarm/src/stan_files/bernoulli.stan"
             test-stan-mode-dir)))))
    ;;
    (it "gives #include font-lock-preprocessor-face"
      (expect
       (test-stan--check-face (rx "#include /pre/Columbia_copyright.stan") src-highlighted)
       :to-be
       'font-lock-preprocessor-face)
      (expect
       (test-stan--check-face (rx "/pre/Columbia_copyright.stan") src-highlighted)
       :to-be
       nil)
      (expect
       (test-stan--check-face (rx "#include /functions/common_functions.stan") src-highlighted)
       :to-be
       'font-lock-preprocessor-face)
      (expect
       (test-stan--check-face (rx "/functions/common_functions.stan") src-highlighted)
       :to-be
       nil))
    ;;
    (it "gives comments correct faces"
      (expect
       (test-stan--check-face (rx "// GLM for a Bernoulli outcome") src-highlighted)
       :to-be
       'font-lock-comment-delimiter-face)
      (expect
       (test-stan--check-face (rx "GLM for a Bernoulli outcome") src-highlighted)
       :to-be
       'font-lock-comment-face)
      (expect
       (test-stan--check-face (rx "// number of predictors") src-highlighted)
       :to-be
       'font-lock-comment-delimiter-face)
      (expect
       (test-stan--check-face (rx "number of predictors") src-highlighted)
       :to-be
       'font-lock-comment-face))
    ;;
    (it "gives block names font-lock-keyword-face"
      (expect
       (test-stan--check-face (rx "functions {") src-highlighted)
       :to-be
       'font-lock-keyword-face)
      (expect
       (test-stan--check-face (rx "data {") src-highlighted)
       :to-be
       'font-lock-keyword-face)
      (expect
       (test-stan--check-face (rx "transformed data {") src-highlighted)
       :to-be
       'font-lock-keyword-face)
      (expect
       (test-stan--check-face (rx "parameters {") src-highlighted)
       :to-be
       'font-lock-keyword-face)
      (expect
       (test-stan--check-face (rx "transformed parameters {") src-highlighted)
       :to-be
       'font-lock-keyword-face)
      (expect
       (test-stan--check-face (rx "model {") src-highlighted)
       :to-be
       'font-lock-keyword-face)
      (expect
       (test-stan--check-face (rx "generated quantities {") src-highlighted)
       :to-be
       'font-lock-keyword-face))
    ;;
    (it "gives type names font-lock-type-face"
      (expect
       (test-stan--check-face (rx "matrix[N[2], K_smooth]") src-highlighted)
       :to-be
       'font-lock-type-face)
      (expect
       (test-stan--check-face (rx "real<upper=") src-highlighted)
       :to-be
       'font-lock-type-face)
      (expect
       (test-stan--check-face (rx "int start") src-highlighted)
       :to-be
       'font-lock-type-face)
      (expect
       (test-stan--check-face (rx "real aux") src-highlighted)
       :to-be
       'font-lock-type-face))
    ;;
    (it "gives range names font-lock-keyword-face"
      (expect
       (test-stan--check-face (rx "upper=") src-highlighted)
       :to-be
       'font-lock-keyword-face))
    ;;
    (it "gives assignments font-lock-constant-face"
      (expect
       (test-stan--check-face (rx "=0, upper") src-highlighted)
       :to-be
       'font-lock-constant-face)
      (expect
       (test-stan--check-face (rx "= rows") src-highlighted)
       :to-be
       'font-lock-constant-face))
    ;;
    (it "gives variables font-lock-variable-name-face"
      (expect
       (test-stan--check-face (rx "K;") src-highlighted)
       :to-be
       'font-lock-variable-name-face)
      (expect
       (test-stan--check-face (rx "xbar;") src-highlighted)
       :to-be
       'font-lock-variable-name-face)
      (expect
       (test-stan--check-face (rx "family;") src-highlighted)
       :to-be
       'font-lock-variable-name-face)
      (expect
       (test-stan--check-face (rx "shift;
        shift") src-highlighted)
       :to-be
       'font-lock-variable-name-face))
    ;;
    (it "gives target += font-lock-keyword-face"
      (expect
       (test-stan--check-face (rx "target") src-highlighted)
       :to-be
       'font-lock-keyword-face))))

(describe "stan-mode font lock (deprecated comments example)"
  (let* ((src-highlighted
          (test-stan--highlight
           "#include /pre/Columbia_copyright.stan
 #include /pre/license.stan
#ixclude /pre/incorrect.stan
# this is a deprecated comment
  # this is also deprecated
# data int lower these should not be highlighted
// GLM for a Bernoulli outcome
functions {
#include /functions/common_functions.stan
 #include /functions/bernoulli_likelihoods.stan
#include \"/functions/double_quote.stan\"
#include '/functions/single_quote.stan'
}
data {
  # dimensions
  int<lower=0> K;        # number of predictors
  int<lower=0> N[2];     #include cannot_be_after_code
  vector[K] xbar;        # vector of column-means of rbind(X0, X1)
  int<lower=0,upper=1> dense_X; # flag for dense vs. sparse
  matrix[N[1],K] X0[dense_X];   # centered (by xbar) predictor matrix | y = 0
  matrix[N[2],K] X1[dense_X];   # centered (by xbar) predictor matrix | y = 1
  print(\"hello
#hello
#include this_is_not_include_directive
there\"
}")))
    ;;
    (describe "gives #include font-lock-preprocessor-face and handle its body"
      (it "#include /pre/Columbia_copyright.stan"
        (expect
         (test-stan--check-face (rx "#include /pre/Columbia_copyright.stan") src-highlighted)
         :to-be
         'font-lock-preprocessor-face))
      (it "         /pre/Columbia_copyright.stan"
        (expect
         (test-stan--check-face (rx "/pre/Columbia_copyright.stan") src-highlighted)
         :to-be
         nil))
      (it " #include /pre/license.stan"
        (expect
         (test-stan--check-face (rx "#include /pre/license.stan") src-highlighted)
         :to-be
         'font-lock-preprocessor-face))
      (it "          /pre/license.stan"
        (expect
         (test-stan--check-face (rx "/pre/license.stan") src-highlighted)
         :to-be
         nil))
      (it "#include /functions/common_functions.stan"
        (expect
         (test-stan--check-face (rx "#include /functions/common_functions.stan") src-highlighted)
         :to-be
         'font-lock-preprocessor-face))
      (it " #include /functions/bernoulli_likelihoods.stan"
        (expect
         (test-stan--check-face (rx "#include /functions/bernoulli_likelihoods.stan") src-highlighted)
         :to-be
         'font-lock-preprocessor-face))
      (it "/functions/common_functions.stan"
        (expect
         (test-stan--check-face (rx "/functions/common_functions.stan") src-highlighted)
         :to-be
         nil))
      (it "/functions/double_quote.stan"
        (expect
         ;; This should appear as a string.
         (test-stan--check-face (rx "/functions/double_quote.stan") src-highlighted)
         :to-be
         'font-lock-string-face))
      (it "/functions/single_quote.stan"
        (expect
         ;; This is ok to be not font-locked.
         (test-stan--check-face (rx "/functions/single_quote.stan") src-highlighted)
         :to-be
         nil))
      (it "#hello within a string"
        (expect
         ;; This is a fake comment within a string.
         (test-stan--check-face (rx "#hello") src-highlighted)
         :to-be
         'font-lock-string-face))
      (it "#include within a string"
        (expect
         ;; This is a fake comment within a string.
         (test-stan--check-face (rx "#include this_is_not_include_directive") src-highlighted)
         :to-be
         'font-lock-string-face))
      (it "text following #include within a string"
        (expect
         ;; This is a fake comment within a string.
         (test-stan--check-face (rx "this_is_not_include_directive") src-highlighted)
         :to-be
         'font-lock-string-face)))
    ;;
    (describe "gives // comments correct faces"
      (it "// GLM for a Bernoulli outcome (font-lock-comment-delimiter-face)"
        (expect
         (test-stan--check-face (rx "// GLM for a Bernoulli outcome") src-highlighted)
         :to-be
         'font-lock-comment-delimiter-face))
      (it "   GLM for a Bernoulli outcome (font-lock-comment-face"
        (expect
         (test-stan--check-face (rx "GLM for a Bernoulli outcome") src-highlighted)
         :to-be
         'font-lock-comment-face)))
    ;;
    (describe "gives deprecated comments correct faces"
      (it "#ixclude /pre/incorrect.stan"
        (expect
         (test-stan--check-face (rx "#ixclude /pre/incorrect.stan") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "# this is a deprecated comment"
        (expect
         (test-stan--check-face (rx "# this is a deprecated comment") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "  this is a deprecated comment"
        (expect
         (test-stan--check-face (rx "this is a deprecated comment") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "# this is also deprecated"
        (expect
         (test-stan--check-face (rx "# this is also deprecated") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "  this is also deprecated"
        (expect
         (test-stan--check-face (rx "this is also deprecated") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "  data int lower these should not be highlighted"
        (expect
         (test-stan--check-face (rx "data int lower these should not be highlighted") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "       int lower these should not be highlighted"
        (expect
         (test-stan--check-face (rx "int lower these should not be highlighted") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "           lower these should not be highlighted"
        (expect
         (test-stan--check-face (rx "lower these should not be highlighted") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "                 these should not be highlighted"
        (expect
         (test-stan--check-face (rx "these should not be highlighted") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "# number of predictors"
        (expect
         (test-stan--check-face (rx "# number of predictors") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "  number of predictors"
        (expect
         (test-stan--check-face (rx "number of predictors") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "code; #include cannot_be_after_code"
        (expect
         (test-stan--check-face (rx "#include cannot_be_after_code") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "# vector of column-means of rbind(X0, X1)"
        (expect
         (test-stan--check-face (rx "# vector of column-means of rbind(X0, X1)") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "  vector of column-means of rbind(X0, X1)"
        (expect
         (test-stan--check-face (rx "vector of column-means of rbind(X0, X1)") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "         of column-means of rbind(X0, X1)"
        (expect
         (test-stan--check-face (rx "of column-means of rbind(X0, X1)") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "# centered (by xbar) predictor matrix | y = 0"
        (expect
         (test-stan--check-face (rx "# centered (by xbar) predictor matrix | y = 0") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "           (by xbar) predictor matrix | y = 0"
        (expect
         (test-stan--check-face (rx "(by xbar) predictor matrix | y = 0") src-highlighted)
         :to-be
         'font-lock-warning-face))
      (it "                               matrix | y = 0"
        (expect
         (test-stan--check-face (rx "matrix | y = 0") src-highlighted)
         :to-be
         'font-lock-warning-face)))))

(describe "stan-mode font lock (rstanarm bernoulli likelihoods example)"
  (let* ((src-highlighted
          (test-stan--highlight
           (concat
            "functions {\n"
            (test-stan--get-string-from-file
             (expand-file-name
              "../rstanarm/src/stan_files/functions/bernoulli_likelihoods.stan"
              test-stan-mode-dir))
            "}"))))
    ;;
    (it "gives function document string correct faces"
      (expect
       (test-stan--check-face (rx "Apply inverse link function to linear predictor") src-highlighted)
       :to-be
       'font-lock-comment-face)
      (expect
       (test-stan--check-face (rx "@param eta Linear predictor vector") src-highlighted)
       :to-be
       'font-lock-keyword-face)
      (expect
       (test-stan--check-face (rx "eta Linear predictor vector") src-highlighted)
       :to-be
       'font-lock-variable-name-face)
      (expect
       (test-stan--check-face (rx "@return A vector, i.e. inverse-link(eta)") src-highlighted)
       :to-be
       'font-lock-keyword-face)
      (expect
       (test-stan--check-face (rx "A vector, i.e. inverse-link(eta)") src-highlighted)
       :to-be
       'font-lock-comment-face))
    ;;
    (it "gives return types correct faces"
      (expect
       (test-stan--check-face (rx "vector linkinv_bern") src-highlighted)
       :to-be
       'font-lock-type-face)
      (expect
       (test-stan--check-face (rx "real ll_bern_lp") src-highlighted)
       :to-be
       'font-lock-type-face)
      (expect
       (test-stan--check-face (rx "vector pw_bern") src-highlighted)
       :to-be
       'font-lock-type-face))
    ;;
    (it "gives user-defined function names correct faces"
      (expect
       (test-stan--check-face (rx "linkinv_bern") src-highlighted)
       :to-be
       'font-lock-variable-name-face)
      (expect
       (test-stan--check-face (rx "ll_bern_lp") src-highlighted)
       :to-be
       'font-lock-variable-name-face)
      (expect
       (test-stan--check-face (rx "pw_bern") src-highlighted)
       :to-be
       'font-lock-variable-name-face))
    ;;
    (it "gives argument correct faces"
      (expect
       (test-stan--check-face (rx "vector eta,") src-highlighted)
       :to-be
       'font-lock-type-face)
      (expect
       (test-stan--check-face (rx "vector eta1,") src-highlighted)
       :to-be
       'font-lock-type-face)
      (expect
       (test-stan--check-face (rx "int y") src-highlighted)
       :to-be
       'font-lock-type-face))))

(describe "stan-mode font lock (stan-lang demo files)"
  (let* ((demo-highlighted
          (test-stan--highlight
           (test-stan--get-string-from-file
            (expand-file-name
             "../stan-language-definitions/examples/demo.stan"
             test-stan-mode-dir))))
         (highlight-test-highlighted
          (test-stan--highlight
           (test-stan--get-string-from-file
            (expand-file-name
             "../stan-language-definitions/examples/highlight-test.stan"
             test-stan-mode-dir)))))
    ;;
    (it "gives truncation the font-lock-keyword-face in demo"
      (expect
       (test-stan--check-face (rx "T[-1., 1.];") demo-highlighted)
       :to-be
       'font-lock-keyword-face)
      (expect
       (test-stan--check-face (rx "T[, 1.];") demo-highlighted)
       :to-be
       'font-lock-keyword-face)
      (expect
       (test-stan--check-face (rx "T[-1., ];") demo-highlighted)
       :to-be
       'font-lock-keyword-face)
      (expect
       (test-stan--check-face (rx "T[ , ];") demo-highlighted)
       :to-be
       'font-lock-keyword-face))
    ;;
    (it "gives no face to test following whitespace #include"
      (expect
       (test-stan--check-face (rx "stuff_should_not_have_warning_face") demo-highlighted)
       :to-be
       nil))
    ;;
    (it "gives leading #include within a string font-lock-string-face"
      (expect
       (test-stan--check-face (rx "#include should not be a preprocessor. #include is within a string.") demo-highlighted)
       :to-be
       'font-lock-string-face))
    ;;
    (it "gives non-leading #include within a string font-lock-string-face"
      (expect
       (test-stan--check-face (rx "#include is within a string.") demo-highlighted)
       :to-be
       'font-lock-string-face))
    ;;
    (it "gives strings font-lock-string-face in highlight-test"
      (expect
       (test-stan--check-face (rx "foo.stan") highlight-test-highlighted)
       :to-be
       'font-lock-string-face)
      (expect
       (test-stan--check-face (rx "Hello world!") highlight-test-highlighted)
       :to-be
       'font-lock-string-face))))

(describe "stan-mode font lock (Word-level comprehensive)"
  (it "gives anything following # font-lock-warning-face unless #include"
    (mapc (lambda (name)
            (expect
             (test-stan--check-face name
                                    (test-stan--highlight name))
             :to-be
             'font-lock-warning-face))
          '("#comment" "# comment" "#  comment"))
    (expect
     (test-stan--check-face (rx "comment")
                            (test-stan--highlight "#comment"))
     :to-be
     'font-lock-warning-face)
    (expect
     (test-stan--check-face (rx "comment")
                            (test-stan--highlight "# comment"))
     :to-be
     'font-lock-warning-face)
    (expect
     (test-stan--check-face (rx "comment")
                            (test-stan--highlight "#  comment"))
     :to-be
     'font-lock-warning-face)
    (expect
     (test-stan--check-face (rx "comment")
                            (test-stan--highlight " # comment"))
     :to-be
     'font-lock-warning-face)
    (expect
     (test-stan--check-face (rx "comment")
                            (test-stan--highlight "  #  comment"))
     :to-be
     'font-lock-warning-face)
    (expect
     (test-stan--check-face (rx "comment")
                            (test-stan--highlight "  y ~ normal(0,1);  # comment"))
     :to-be
     'font-lock-warning-face))
  ;;
  (it "gives anything following # font-lock-warning-face even if they are keywords"
    ;; A valid keyword can appear differently if the priority is wrong.
    (expect
     (test-stan--check-face (rx "normal")
                            (test-stan--highlight "# normal"))
     :to-be
     'font-lock-warning-face)
    ;; A deprecated keyword can mess up the following word's face.
    (expect
     (test-stan--check-face (rx "afterlp")
                            (test-stan--highlight "# get_lp afterlp"))
     :to-be
     'font-lock-warning-face))
  ;;
  (it "does not interfere with # in a string"
    (expect
     (test-stan--check-face (rx "normal")
                            (test-stan--highlight "  \"# normal\""))
     :to-be
     'font-lock-string-face)
    (expect
     (test-stan--check-face (rx "normal")
                            (test-stan--highlight "  \"# normal # text\""))
     :to-be
     'font-lock-string-face)
    (expect
     (test-stan--check-face (rx "normal")
                            (test-stan--highlight "  \" # normal # text\""))
     :to-be
     'font-lock-string-face))
  ;;
  (it "gives all block names default if not followed by {"
    (mapc (lambda (name)
            (expect
             (test-stan--check-face name
                                    (test-stan--highlight name))
             :to-be
             nil))
          stan-keywords--blocks-list))
  ;;
  (it "gives all block names font-lock-keyword-face if followed by {"
    (mapc (lambda (name)
            (expect
             (test-stan--check-face name
                                    (test-stan--highlight
                                     (concat name
                                             " {")))
             :to-be
             'font-lock-keyword-face))
          stan-keywords--blocks-list))
  ;;
  (it "gives all type names font-lock-type-face"
    (mapc (lambda (name)
            (expect
             (test-stan--check-face name
                                    (test-stan--highlight
                                     (concat name
                                             " {")))
             :to-be
             'font-lock-type-face))
          (append stan-keywords--types-list
                  stan-keywords--function-return-types-list)))
  ;;
  (it "gives all function names font-lock-function-name-face"
    (mapc (lambda (name)
            (expect
             (test-stan--check-face name
                                    (test-stan--highlight
                                     name))
             :to-be
             'font-lock-function-name-face))
          ;; Need to remove target() which is treated as a keyword.
          (seq-filter (lambda (str)
                        (not (equal "target" str)))
                      stan-keywords--functions-list)))
  ;;
  (it "gives all distribution names font-lock-function-name-face if not after ~"
    (mapc (lambda (name)
            (expect
             (test-stan--check-face name
                                    (test-stan--highlight
                                     name))
             :to-be
             nil))
          stan-keywords--distribution-list))
  ;;
  (it "gives all distribution names font-lock-function-name-face if after ~"
    (mapc (lambda (name)
            (expect
             (test-stan--check-face name
                                    (test-stan--highlight
                                     (concat "~ "
                                             name)))
             :to-be
             'font-lock-function-name-face))
          stan-keywords--distribution-list))
  ;;
  (it "gives all reserved keywords font-lock-warning-face"
    (mapc (lambda (name)
            (expect
             (test-stan--check-face name
                                    (test-stan--highlight
                                     name))
             :to-be
             'font-lock-warning-face))
          ;; Need to remove these
          (seq-filter (lambda (str)
                        (not (or (equal "int" str)
                                 (equal "void" str))))
                      stan-keywords--reserved-list)))
  ;;
  (it "gives all deprecated items font-lock-warning-face"
    (mapc (lambda (name)
            (expect
             (test-stan--check-face name
                                    (test-stan--highlight
                                     name))
             :to-be
             'font-lock-warning-face))
          (append '("<-" "__" "a__" "ab__")
                  stan-keywords--deprecated-function-list)))
  ;;
  (it "gives all assignment operators font-lock-constant-face"
    (mapc (lambda (name)
            (expect
             (test-stan--check-face name
                                    (test-stan--highlight
                                     name))
             :to-be
             'font-lock-constant-face))
          '("=" "~" "+=")))
  ;;
  (it "gives all keywords font-lock-keyword-face"
    (mapc (lambda (name)
            (expect
             (test-stan--check-face name
                                    (test-stan--highlight
                                     name))
             :to-be
             'font-lock-keyword-face))
          stan-keywords--keywords-list))
  ;;
  (it "gives lower and upper font-lock-keyword-face only in context"
    (expect
     (test-stan--check-face (rx "lower")
                            (test-stan--highlight "lower=0"))
     :to-be
     nil)
    (expect
     (test-stan--check-face (rx "lower")
                            (test-stan--highlight "<lower=0,"))
     :to-be
     'font-lock-keyword-face)
    (expect
     (test-stan--check-face (rx "upper")
                            (test-stan--highlight "upper=1"))
     :to-be
     nil)
    (expect
     (test-stan--check-face (rx "upper")
                            (test-stan--highlight "<lower=0,upper=1>"))
     :to-be
     'font-lock-keyword-face)))


;;; c++-mode check
;; stan-mode may interfere with c++-mode syntax highlighting
;; https://github.com/stan-dev/stan-mode/issues/64
(defun test-stan--highlight-c++ (src)
  "Syntax hightlight SRC with `c++-mode'.

Adopted from test/groovy-unit-test.el in the `groovy-mode'."
  (with-temp-buffer
    (insert src)
    (goto-char (point-min))
    (c++-mode)
    ;; Ensure we've syntax-highlighted the whole buffer.
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings
        (font-lock-fontify-buffer)))
    (buffer-string)))

(provide 'test-stan-mode)
;;; test-stan-mode.el ends here
