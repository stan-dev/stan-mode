;;; test-company-stan.el --- A buttercup test suite for company-stan -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kazuki Yoshida

;; Author: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; URL: https://github.com/stan-dev/stan-mode/tree/master/company-stan
;; Keywords: languages
;; Version: 10.4.0
;; Created: 2019-07-26
;; Package-Requires: ((emacs "24") (company "0.9.10") (stan-mode "10.4.0"))

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

;;; Code:
(require 'buttercup)
(require 'company-stan)


(describe "company-stan--propertize-list"
  ;;
  (let* ((list-of-strings '("s-a" "str-b" "string-c"))
         (list-of-strings-propertized
          (company-stan--propertize-list list-of-strings
                                         "this-category")))
    (it "gives each string in a list :category property"
      (expect
       (mapcar
        (lambda (str) (get-text-property 0 :category str))
        list-of-strings-propertized)
       :to-equal
       '("this-category" "this-category" "this-category")))))


(describe "company-stan--fuzzy-match"
  (it "returns true if prefix matching start of candidate"
    (expect
     (company-stan--fuzzy-match "abc" "abcde")
     :to-be-truthy))
  ;;
  (it "returns true if prefix skipping characters in candidate"
    (expect
     (company-stan--fuzzy-match "ace" "abcde")
     :to-be-truthy))
  ;;
  (it "returns false if prefix characters not in candidate"
    (expect
     ;; f is not in the candidate
     (company-stan--fuzzy-match "acf" "abcde")
     :not :to-be-truthy))
  ;;
  (it "returns true in order agnostic manner"
    (expect
     (company-stan--fuzzy-match "ba" "abcde")
     :to-be-truthy)))


(describe "company-stan-backend-annotation"
  (let* ((list-of-strings '("s-a" "str-b" "string-c"))
         (list-of-strings-propertized
          (company-stan--propertize-list list-of-strings
                                         "this-category")))
    ;;
    (it "constructs an annotation string SPC[%s] from :category property"
      (expect
       (company-stan-backend-annotation (nth 0 list-of-strings-propertized))
       :to-equal
       " [this-category]")
      ;;
      (expect
       (company-stan-backend-annotation (nth 1 list-of-strings-propertized))
       :to-equal
       " [this-category]"))))


(describe "company-complete in stan-mode"
  (it "populates company-candidates correctly after norm"
    (with-temp-buffer
      (stan-mode)
      (company-stan-setup)
      (company-mode)
      (insert "norm")
      (company-complete)
      ;;
      (expect
       (member "normal"
               company-candidates)
       :to-be-truthy)
      (expect
       (member "normal_lpdf"
               company-candidates)
       :to-be-truthy)
      (expect
       (member "normal_lpmf"
               company-candidates)
       :not :to-be-truthy)
      (expect
       (member "cauchy_lpdf"
               company-candidates)
       :not :to-be-truthy)))
  ;;
  (it "populates company-candidates correctly after bern"
    (with-temp-buffer
      (stan-mode)
      (company-stan-setup)
      (company-mode)
      (insert "bern")
      (company-complete)
      ;;
      (expect
       (member "bernoulli"
               company-candidates)
       :to-be-truthy)
      (expect
       (member "bernoulli_lpmf"
               company-candidates)
       :to-be-truthy)
      (expect
       (member "bernoulli_lpdf"
               company-candidates)
       :not :to-be-truthy)
      (expect
       (member "cauchy_lpdf"
               company-candidates)
       :not :to-be-truthy))))


(xdescribe "company-stan--which-block"
  (with-temp-buffer
    (stan-mode)
    (company-stan-setup)
    (company-mode)
    (insert "// place_before_blocks
functions {
  // place_functions
}

data {
  // place_data
  // Number of observations
  int<lower=1> N;
  // Whether to evaluate likelihood
  int<lower=0,upper=1> use_lik;
}
// place_between_blocks
transformed  data {
  // place_transformed_data
}

   parameters {
     // place_parameters
}

transformed   parameters {
  // place_transformed_parameters
}

model {
  // Priors

  // Likelihood
  if (use_lik == 1) {
    // place_model
  }
}

generated quantities  {
  // For loo
  vector[N] log_lik;
  // For posterior predictive checks
  real y_rep[N];

  for (i in 1:N) {
    // Observation level log likelihood
    log_lik[i] = ;
    // place_generated_quantities
    // Prediction
    y_rep[i] = ;
  }
}")
    ;;
    (it "gives nil before blocks"
      (expect
       (progn
         (goto-char 0)
         (search-forward "place_before_blocks")
         (company-stan--which-block))
       :to-equal
       nil))
    ;;
    (it "gives nil between blocks"
      (expect
       (progn
         (goto-char 0)
         (search-forward "place_between_blocks")
         (company-stan--which-block))
       :to-equal
       nil))
    ;;
    (it "gives functions when in it"
      (expect
       (progn
         (goto-char 0)
         (re-search-forward "place_functions")
         (company-stan--which-block))
       :to-equal
       "functions"))
    ;;
    (it "gives data when in it"
      (expect
       (progn
         (goto-char 0)
         (re-search-forward "place_data")
         (company-stan--which-block))
       :to-equal
       "data"))
    ;;
    (it "gives transformed_data when in it"
      (expect
       (progn
         (goto-char 0)
         (re-search-forward "place_transformed_data")
         (company-stan--which-block))
       :to-equal
       "transformed_data"))
    ;;
    (it "gives parameters when in it"
      (expect
       (progn
         (goto-char 0)
         (re-search-forward "place_parameters")
         (company-stan--which-block))
       :to-equal
       "parameters"))
    ;;
    (it "gives transformed_parameters when in it"
      (expect
       (progn
         (goto-char 0)
         (re-search-forward "place_transformed_parameters")
         (company-stan--which-block))
       :to-equal
       "transformed_parameters"))
    ;;
    (it "gives model when in it"
      (expect
       (progn
         (goto-char 0)
         (re-search-forward "place_model")
         (company-stan--which-block))
       :to-equal
       "model"))
    ;;
    (it "gives generated_quantities when in it"
      (expect
       (progn
         (goto-char 0)
         (re-search-forward "place_generated_quantities")
         (company-stan--which-block))
       :to-equal
       "generated_quantities"))))


(xdescribe "company-complete in stan-mode (context-aware)"
  ;;
  (it "populates company-candidates with distributions after ~ norm"
    (with-temp-buffer
      (stan-mode)
      (company-stan-setup)
      (company-mode)
      (insert "mu ~ norm")
      (company-complete)
      ;;
      (expect
       (member "normal"
               company-candidates)
       :to-be-truthy)
      (expect
       (member "normal_lpdf"
               company-candidates)
       :not :to-be-truthy)
      (expect
       (member "normal_lcdf"
               company-candidates)
       :not :to-be-truthy)
      (expect
       (member "cauchy_lpdf"
               company-candidates)
       :not :to-be-truthy)))
  ;;
  (it "populates company-candidates with *_rng in generated quantities only"
    (with-temp-buffer
      (stan-mode)
      (company-stan-setup)
      (company-mode)
      (insert "model {
  target += normal_
}")
      (goto-char 0)
      (search-forward "normal_")
      (company-complete)
      (expect
       (member "normal_rng"
               company-candidates)
       :not :to-be-truthy))
    ;;
    (with-temp-buffer
      (stan-mode)
      (company-stan-setup)
      (company-mode)
      (insert "generated quantities {
  y_rep = normal_
}")
      (goto-char 0)
      (search-forward "normal_")
      (company-complete)
      (expect
       (member "normal_rng"
               company-candidates)
       :to-be-truthy))))


(provide 'test-company-stan)
;;; test-company-stan.el ends here
