;;; test-eldoc-stan-create-json.el --- A buttercup test suite for eldoc-stan-create-json -*- lexical-binding: t; -*-

;; Author: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; URL: https://github.com/stan-dev/stan-mode/tree/master/eldoc-stan
;; Keywords: help, tools
;; Version: 10.0.0
;; Created: 2019-07-29
;; Package-Requires: ((emacs "25.1") (stan-mode "10.0.0"))

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
(require 'eldoc-stan-create-json)


;;; The following tests are for lookup data generation functions.
(describe "eldoc-stan-create-json--string-join-with-comma"
  ;;
  (it "joins a list of strings with commas in between"
    (expect
     (eldoc-stan-create-json--string-join-with-comma nil '("a" "b" "c"))
     :to-equal
     "a, b, c"))
  ;;
  (it "drops the first element if asked"
    (expect
     (eldoc-stan-create-json--string-join-with-comma t '("a" "b" "c"))
     :to-equal
     "b, c"))
  ;;
  (it "uses | separator between the first and second elements if asked"
    (expect
     (eldoc-stan-create-json--string-join-with-comma 'given '("a" "b" "c"))
     :to-equal
     "a | b, c")))


(describe "eldoc-stan-create-json--signature-to-string"
  (let* ((neg_binomial_2_log_glm_lpmf
          ;; This string was created using (json-encode ...)
          "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"int[]\",
          \"name\": \"y\"},
        {
          \"type\": \"matrix\",
          \"name\": \"x\"},
        {
          \"type\": \"real\",
          \"name\": \"alpha\"},
        {
          \"type\": \"vector\",
          \"name\": \"beta\"},
        {
          \"type\": \"real\",
          \"name\": \"phi\"}]},
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"int[]\",
          \"name\": \"y\"},
        {
          \"type\": \"matrix\",
          \"name\": \"x\"},
        {
          \"type\": \"vector\",
          \"name\": \"alpha\"},
        {
          \"type\": \"vector\",
          \"name\": \"beta\"},
        {
          \"type\": \"real\",
          \"name\": \"phi\"}]}],
  \"sampling\": \"neg_binomial_2_log_glm\",
  \"operator\": false,
  \"math\": false,
  \"lpmf\": true,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": true}")
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (neg_binomial_2_log_glm_lpmf-hash
          (json-read-from-string neg_binomial_2_log_glm_lpmf))
         (neg_binomial_2_log_glm_lpmf-sigatures
          (gethash "signatures" neg_binomial_2_log_glm_lpmf-hash)))
    ;;
    (it "transforms a signature to a string"
      (expect
       (eldoc-stan-create-json--signature-to-string
        (nth 0 neg_binomial_2_log_glm_lpmf-sigatures)
        nil)
       :to-equal
       "int[] y, matrix x, real alpha, vector beta, real phi")
      ;;
      (expect
       (eldoc-stan-create-json--signature-to-string
        (nth 1 neg_binomial_2_log_glm_lpmf-sigatures)
        nil)
       :to-equal
       "int[] y, matrix x, vector alpha, vector beta, real phi"))
    ;;
    (it "drops the first element when asked"
      (expect
       (eldoc-stan-create-json--signature-to-string
        (nth 0 neg_binomial_2_log_glm_lpmf-sigatures)
        t)
       :to-equal
       "matrix x, real alpha, vector beta, real phi")
      ;;
      (expect
       (eldoc-stan-create-json--signature-to-string
        (nth 1 neg_binomial_2_log_glm_lpmf-sigatures)
        t)
       :to-equal
       "matrix x, vector alpha, vector beta, real phi"))
    ;;
    (it "uses | between the first and second elements when asked"
      (expect
       (eldoc-stan-create-json--signature-to-string
        (nth 0 neg_binomial_2_log_glm_lpmf-sigatures)
        'given)
       :to-equal
       "int[] y | matrix x, real alpha, vector beta, real phi")
      ;;
      (expect
       (eldoc-stan-create-json--signature-to-string
        (nth 1 neg_binomial_2_log_glm_lpmf-sigatures)
        'given)
       :to-equal
       "int[] y | matrix x, vector alpha, vector beta, real phi"))))


(describe "eldoc-stan-create-json--signatures-to-list-of-strings"
  (let* ((neg_binomial_2_log_glm_lpmf
          ;; This string was created using (json-encode ...)
          "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"int[]\",
          \"name\": \"y\"},
        {
          \"type\": \"matrix\",
          \"name\": \"x\"},
        {
          \"type\": \"real\",
          \"name\": \"alpha\"},
        {
          \"type\": \"vector\",
          \"name\": \"beta\"},
        {
          \"type\": \"real\",
          \"name\": \"phi\"}]},
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"int[]\",
          \"name\": \"y\"},
        {
          \"type\": \"matrix\",
          \"name\": \"x\"},
        {
          \"type\": \"vector\",
          \"name\": \"alpha\"},
        {
          \"type\": \"vector\",
          \"name\": \"beta\"},
        {
          \"type\": \"real\",
          \"name\": \"phi\"}]}],
  \"sampling\": \"neg_binomial_2_log_glm\",
  \"operator\": false,
  \"math\": false,
  \"lpmf\": true,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": true}")
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (neg_binomial_2_log_glm_lpmf-hash
          (json-read-from-string neg_binomial_2_log_glm_lpmf))
         (neg_binomial_2_log_glm_lpmf-sigatures
          (gethash "signatures" neg_binomial_2_log_glm_lpmf-hash)))
    ;;
    (it "transforms signatures to a list of strings"
      (expect
       (eldoc-stan-create-json--signatures-to-list-of-strings
        neg_binomial_2_log_glm_lpmf-sigatures
        nil)
       :to-equal
       '("int[] y, matrix x, real alpha, vector beta, real phi"
         "int[] y, matrix x, vector alpha, vector beta, real phi")))
    ;;
    (it "drops the first element when asked"
      (expect
       (eldoc-stan-create-json--signatures-to-list-of-strings
        neg_binomial_2_log_glm_lpmf-sigatures
        t)
       :to-equal
       '("matrix x, real alpha, vector beta, real phi"
         "matrix x, vector alpha, vector beta, real phi")))
    ;;
    (it "uses | separator between the first and second elements if asked"
      (expect
       (eldoc-stan-create-json--signatures-to-list-of-strings
        neg_binomial_2_log_glm_lpmf-sigatures
        'given)
       :to-equal
       '("int[] y | matrix x, real alpha, vector beta, real phi"
         "int[] y | matrix x, vector alpha, vector beta, real phi")))))


(describe "eldoc-stan-create-json--density-function?"
  ;; These strings were created using (json-encode ...)
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (normal_lpdf-string "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"reals\",
          \"name\": \"y\"},
        {
          \"type\": \"reals\",
          \"name\": \"mu\"},
        {
          \"type\": \"reals\",
          \"name\": \"sigma\"}]}],
  \"sampling\": \"normal\",
  \"operator\": false,
  \"math\": false,
  \"lpmf\": false,
  \"lpdf\": true,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": true}")
         (normal_rng-string "{
  \"signatures\": [
    {
      \"return\": \"R\",
      \"args\": [
        {
          \"type\": \"reals\",
          \"name\": \"mu\"},
        {
          \"type\": \"reals\",
          \"name\": \"sigma\"}]}],
  \"sampling\": null,
  \"operator\": false,
  \"math\": true,
  \"lpmf\": false,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": false}")
         (poisson_lpmf-string "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"ints\",
          \"name\": \"n\"},
        {
          \"type\": \"reals\",
          \"name\": \"lambda\"}]}],
  \"sampling\": \"poisson\",
  \"operator\": false,
  \"math\": false,
  \"lpmf\": true,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": true}")
         (poisson_rng-string "{
  \"signatures\": [
    {
      \"return\": \"R\",
      \"args\": [
        {
          \"type\": \"reals\",
          \"name\": \"lambda\"}]}],
  \"sampling\": null,
  \"operator\": false,
  \"math\": true,
  \"lpmf\": false,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": false}")
         (normal_lpdf-hash-table (json-read-from-string normal_lpdf-string))
         (normal_rng-hash-table (json-read-from-string normal_rng-string))
         (poisson_lpmf-hash-table (json-read-from-string poisson_lpmf-string))
         (poisson_rng-hash-table (json-read-from-string poisson_rng-string)))
    ;;
    (it "evaluates to t for normal_lpdf"
      (expect
       (eldoc-stan-create-json--density-function? normal_lpdf-hash-table)
       :to-be-truthy))
    ;;
    (it "evaluates to nil for normal_rng"
      (expect
       (eldoc-stan-create-json--density-function? normal_rng-hash-table)
       :not :to-be-truthy))
    ;;
    (it "evaluates to t for poisson_lpmf"
      (expect
       (eldoc-stan-create-json--density-function? poisson_lpmf-hash-table)
       :to-be-truthy))
    ;;
    (it "evaluates to nil for poisson_rng"
      (expect
       (eldoc-stan-create-json--density-function? poisson_rng-hash-table)
       :not :to-be-truthy))))


(describe "eldoc-stan-create-json--lcdf-function?"
  ;; These strings were created using (json-encode ...)
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (normal_lpdf-string "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"reals\",
          \"name\": \"y\"},
        {
          \"type\": \"reals\",
          \"name\": \"mu\"},
        {
          \"type\": \"reals\",
          \"name\": \"sigma\"}]}],
  \"sampling\": \"normal\",
  \"operator\": false,
  \"math\": false,
  \"lpmf\": false,
  \"lpdf\": true,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": true}")
         (normal_lcdf-string "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"reals\",
          \"name\": \"y\"},
        {
          \"type\": \"reals\",
          \"name\": \"mu\"},
        {
          \"type\": \"reals\",
          \"name\": \"sigma\"}]}],
  \"sampling\": null,
  \"operator\": false,
  \"math\": false,
  \"lpmf\": false,
  \"lpdf\": false,
  \"lcdf\": true,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": false}")
         (normal_lccdf-string "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"reals\",
          \"name\": \"y\"},
        {
          \"type\": \"reals\",
          \"name\": \"mu\"},
        {
          \"type\": \"reals\",
          \"name\": \"sigma\"}]}],
  \"sampling\": null,
  \"operator\": false,
  \"math\": false,
  \"lpmf\": false,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": true,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": false}")
         (normal_rng-string "{
  \"signatures\": [
    {
      \"return\": \"R\",
      \"args\": [
        {
          \"type\": \"reals\",
          \"name\": \"mu\"},
        {
          \"type\": \"reals\",
          \"name\": \"sigma\"}]}],
  \"sampling\": null,
  \"operator\": false,
  \"math\": true,
  \"lpmf\": false,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": false}")
         (poisson_lpmf-string "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"ints\",
          \"name\": \"n\"},
        {
          \"type\": \"reals\",
          \"name\": \"lambda\"}]}],
  \"sampling\": \"poisson\",
  \"operator\": false,
  \"math\": false,
  \"lpmf\": true,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": true}")
         (poisson_lcdf-string "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"ints\",
          \"name\": \"n\"},
        {
          \"type\": \"reals\",
          \"name\": \"lambda\"}]}],
  \"sampling\": null,
  \"operator\": false,
  \"math\": false,
  \"lpmf\": false,
  \"lpdf\": false,
  \"lcdf\": true,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": false}")
         (poisson_lccdf-string "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"ints\",
          \"name\": \"n\"},
        {
          \"type\": \"reals\",
          \"name\": \"lambda\"}]}],
  \"sampling\": null,
  \"operator\": false,
  \"math\": false,
  \"lpmf\": false,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": true,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": false}")
         (poisson_rng-string "{
  \"signatures\": [
    {
      \"return\": \"R\",
      \"args\": [
        {
          \"type\": \"reals\",
          \"name\": \"lambda\"}]}],
  \"sampling\": null,
  \"operator\": false,
  \"math\": true,
  \"lpmf\": false,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": false}")
         (normal_lpdf-hash-table (json-read-from-string normal_lpdf-string))
         (normal_lcdf-hash-table (json-read-from-string normal_lcdf-string))
         (normal_lccdf-hash-table (json-read-from-string normal_lccdf-string))
         (normal_rng-hash-table (json-read-from-string normal_rng-string))
         (poisson_lpmf-hash-table (json-read-from-string poisson_lpmf-string))
         (poisson_lcdf-hash-table (json-read-from-string poisson_lcdf-string))
         (poisson_lccdf-hash-table (json-read-from-string poisson_lccdf-string))
         (poisson_rng-hash-table (json-read-from-string poisson_rng-string)))
    ;;
    (it "evaluates to nil for normal_lpdf"
      (expect
       (eldoc-stan-create-json--lcdf-function? normal_lpdf-hash-table)
       :not :to-be-truthy))
    ;;
    (it "evaluates to t for normal_lcdf"
      (expect
       (eldoc-stan-create-json--lcdf-function? normal_lcdf-hash-table)
       :to-be-truthy))
    ;;
    (it "evaluates to nil for normal_lccdf"
      (expect
       (eldoc-stan-create-json--lcdf-function? normal_lccdf-hash-table)
       :not :to-be-truthy))
    ;;
    (it "evaluates to nil for normal_rng"
      (expect
       (eldoc-stan-create-json--lcdf-function? normal_rng-hash-table)
       :not :to-be-truthy))
    ;;
    (it "evaluates to nil for poisson_lpmf"
      (expect
       (eldoc-stan-create-json--lcdf-function? poisson_lpmf-hash-table)
       :not :to-be-truthy))
    ;;
    (it "evaluates to t for poisson_lcdf"
      (expect
       (eldoc-stan-create-json--lcdf-function? poisson_lcdf-hash-table)
       :to-be-truthy))
    ;;
    (it "evaluates to nil for poisson_lccdf"
      (expect
       (eldoc-stan-create-json--lcdf-function? poisson_lccdf-hash-table)
       :not :to-be-truthy))
    ;;
    (it "evaluates to nil for poisson_rng"
      (expect
       (eldoc-stan-create-json--lcdf-function? poisson_rng-hash-table)
       :not :to-be-truthy))))


(describe "eldoc-stan-create-json--lccdf-function?"
  ;; These strings were created using (json-encode ...)
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (normal_lpdf-string "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"reals\",
          \"name\": \"y\"},
        {
          \"type\": \"reals\",
          \"name\": \"mu\"},
        {
          \"type\": \"reals\",
          \"name\": \"sigma\"}]}],
  \"sampling\": \"normal\",
  \"operator\": false,
  \"math\": false,
  \"lpmf\": false,
  \"lpdf\": true,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": true}")
         (normal_lcdf-string "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"reals\",
          \"name\": \"y\"},
        {
          \"type\": \"reals\",
          \"name\": \"mu\"},
        {
          \"type\": \"reals\",
          \"name\": \"sigma\"}]}],
  \"sampling\": null,
  \"operator\": false,
  \"math\": false,
  \"lpmf\": false,
  \"lpdf\": false,
  \"lcdf\": true,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": false}")
         (normal_lccdf-string "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"reals\",
          \"name\": \"y\"},
        {
          \"type\": \"reals\",
          \"name\": \"mu\"},
        {
          \"type\": \"reals\",
          \"name\": \"sigma\"}]}],
  \"sampling\": null,
  \"operator\": false,
  \"math\": false,
  \"lpmf\": false,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": true,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": false}")
         (normal_rng-string "{
  \"signatures\": [
    {
      \"return\": \"R\",
      \"args\": [
        {
          \"type\": \"reals\",
          \"name\": \"mu\"},
        {
          \"type\": \"reals\",
          \"name\": \"sigma\"}]}],
  \"sampling\": null,
  \"operator\": false,
  \"math\": true,
  \"lpmf\": false,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": false}")
         (poisson_lpmf-string "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"ints\",
          \"name\": \"n\"},
        {
          \"type\": \"reals\",
          \"name\": \"lambda\"}]}],
  \"sampling\": \"poisson\",
  \"operator\": false,
  \"math\": false,
  \"lpmf\": true,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": true}")
         (poisson_lcdf-string "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"ints\",
          \"name\": \"n\"},
        {
          \"type\": \"reals\",
          \"name\": \"lambda\"}]}],
  \"sampling\": null,
  \"operator\": false,
  \"math\": false,
  \"lpmf\": false,
  \"lpdf\": false,
  \"lcdf\": true,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": false}")
         (poisson_lccdf-string "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"ints\",
          \"name\": \"n\"},
        {
          \"type\": \"reals\",
          \"name\": \"lambda\"}]}],
  \"sampling\": null,
  \"operator\": false,
  \"math\": false,
  \"lpmf\": false,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": true,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": false}")
         (poisson_rng-string "{
  \"signatures\": [
    {
      \"return\": \"R\",
      \"args\": [
        {
          \"type\": \"reals\",
          \"name\": \"lambda\"}]}],
  \"sampling\": null,
  \"operator\": false,
  \"math\": true,
  \"lpmf\": false,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": false}")
         (normal_lpdf-hash-table (json-read-from-string normal_lpdf-string))
         (normal_lcdf-hash-table (json-read-from-string normal_lcdf-string))
         (normal_lccdf-hash-table (json-read-from-string normal_lccdf-string))
         (normal_rng-hash-table (json-read-from-string normal_rng-string))
         (poisson_lpmf-hash-table (json-read-from-string poisson_lpmf-string))
         (poisson_lcdf-hash-table (json-read-from-string poisson_lcdf-string))
         (poisson_lccdf-hash-table (json-read-from-string poisson_lccdf-string))
         (poisson_rng-hash-table (json-read-from-string poisson_rng-string)))
    ;;
    (it "evaluates to nil for normal_lpdf"
      (expect
       (eldoc-stan-create-json--lccdf-function? normal_lpdf-hash-table)
       :not :to-be-truthy))
    ;;
    (it "evaluates to nil for normal_lcdf"
      (expect
       (eldoc-stan-create-json--lccdf-function? normal_lcdf-hash-table)
       :not :to-be-truthy))
    ;;
    (it "evaluates to t for normal_lccdf"
      (expect
       (eldoc-stan-create-json--lccdf-function? normal_lccdf-hash-table)
       :to-be-truthy))
    ;;
    (it "evaluates to nil for normal_rng"
      (expect
       (eldoc-stan-create-json--lccdf-function? normal_rng-hash-table)
       :not :to-be-truthy))
    ;;
    (it "evaluates to nil for poisson_lpmf"
      (expect
       (eldoc-stan-create-json--lccdf-function? poisson_lpmf-hash-table)
       :not :to-be-truthy))
    ;;
    (it "evaluates to nil for poisson_lcdf"
      (expect
       (eldoc-stan-create-json--lccdf-function? poisson_lcdf-hash-table)
       :not :to-be-truthy))
    ;;
    (it "evaluates to t for poisson_lccdf"
      (expect
       (eldoc-stan-create-json--lccdf-function? poisson_lccdf-hash-table)
       :to-be-truthy))
    ;;
    (it "evaluates to nil for poisson_rng"
      (expect
       (eldoc-stan-create-json--lccdf-function? poisson_rng-hash-table)
       :not :to-be-truthy))))


(describe "eldoc-stan-create-json--extract-sampling-function"
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (normal_lpdf-string "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"reals\",
          \"name\": \"y\"},
        {
          \"type\": \"reals\",
          \"name\": \"mu\"},
        {
          \"type\": \"reals\",
          \"name\": \"sigma\"}]}],
  \"sampling\": \"normal\",
  \"operator\": false,
  \"math\": false,
  \"lpmf\": false,
  \"lpdf\": true,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": true}")
         (normal_rng-string "{
  \"signatures\": [
    {
      \"return\": \"R\",
      \"args\": [
        {
          \"type\": \"reals\",
          \"name\": \"mu\"},
        {
          \"type\": \"reals\",
          \"name\": \"sigma\"}]}],
  \"sampling\": null,
  \"operator\": false,
  \"math\": true,
  \"lpmf\": false,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": false}")
         (poisson_lpmf-string "{
  \"signatures\": [
    {
      \"return\": \"real\",
      \"args\": [
        {
          \"type\": \"ints\",
          \"name\": \"n\"},
        {
          \"type\": \"reals\",
          \"name\": \"lambda\"}]}],
  \"sampling\": \"poisson\",
  \"operator\": false,
  \"math\": false,
  \"lpmf\": true,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": true}")
         (poisson_rng-string "{
  \"signatures\": [
    {
      \"return\": \"R\",
      \"args\": [
        {
          \"type\": \"reals\",
          \"name\": \"lambda\"}]}],
  \"sampling\": null,
  \"operator\": false,
  \"math\": true,
  \"lpmf\": false,
  \"lpdf\": false,
  \"lcdf\": false,
  \"lccdf\": false,
  \"keyword\": false,
  \"deprecated\": false,
  \"density\": false}")
         (normal_lpdf-hash-table (json-read-from-string normal_lpdf-string))
         (normal_rng-hash-table (json-read-from-string normal_rng-string))
         (poisson_lpmf-hash-table (json-read-from-string poisson_lpmf-string))
         (poisson_rng-hash-table (json-read-from-string poisson_rng-string)))
    ;;
    (it "extract normal given normal_lpdf hash table"
      (expect
       (eldoc-stan-create-json--extract-sampling-function normal_lpdf-hash-table)
       :to-equal
       "normal"))
    ;;
    (it "extract json-null given normal_rng hash table"
      (expect
       (eldoc-stan-create-json--extract-sampling-function normal_rng-hash-table)
       :to-equal
       json-null))
    ;;
    (it "extract poisson given poisson_lpmf hash table"
      (expect
       (eldoc-stan-create-json--extract-sampling-function poisson_lpmf-hash-table)
       :to-equal
       "poisson"))
    ;;
    (it "extract json-null given poisson_rng hash table"
      (expect
       (eldoc-stan-create-json--extract-sampling-function poisson_rng-hash-table)
       :to-equal
       json-null))))


(describe "eldoc-stan-create-json--create-function-hash-table"
  (let* ((stan-lang-json-path
          "../stan-language-definitions/stan_lang.json")
         (ht (eldoc-stan-create-json--create-function-hash-table
              stan-lang-json-path
              nil))
         (ht-given (eldoc-stan-create-json--create-function-hash-table
                    stan-lang-json-path
                    t))
         (fun-names (hash-table-keys ht))
         ;; Extract by patterns
         (fun-names-lpdf
          (seq-filter (lambda (elt) (string-match "_lpdf$" elt))
                      fun-names))
         (fun-names-lpmf
          (seq-filter (lambda (elt) (string-match "_lpmf$" elt))
                      fun-names))
         (fun-names-lcdf
          (seq-filter (lambda (elt) (string-match "_lcdf$" elt))
                      fun-names))
         (fun-names-lccdf
          (seq-filter (lambda (elt) (string-match "_lccdf$" elt))
                      fun-names))
         (fun-names-others
          (seq-filter (lambda (elt)
                        (not (or (string-match "_lpdf$" elt)
                                 (string-match "_lpmf$" elt)
                                 (string-match "_lcdf$" elt)
                                 (string-match "_lccdf$" elt))))
                      fun-names)))
    ;;
    (it "creates a hash table"
      (expect
       (hash-table-p ht)
       :to-be-truthy)
      (expect
       (hash-table-p ht-given)
       :to-be-truthy))
    ;;
    (it "uses | for all *_lpdf arguments when given is true"
      (expect
       (seq-every-p
        #'identity
        ;; Loop over functions
        (mapcar (lambda (fun)
                  ;; Work on sequence of argument strings for each function
                  (thread-last (gethash fun ht-given)
                    (mapcar (lambda (str) (string-match " | " str)))
                    ;; should be a list with non-nil elements only
                    (seq-every-p #'identity)))
                fun-names-lpdf))
       :to-be-truthy))
    ;;
    (it "uses | for all *_lpmf arguments when given is true"
      (expect
       (seq-every-p
        #'identity
        ;; Loop over functions
        (mapcar (lambda (fun)
                  ;; Work on sequence of argument strings for each function
                  (thread-last (gethash fun ht-given)
                    (mapcar (lambda (str) (string-match " | " str)))
                    ;; should be a list with non-nil elements only
                    (seq-every-p #'identity)))
                fun-names-lpmf))
       :to-be-truthy))
    ;;
    (it "uses | for all *_lcdf arguments when given is true"
      (expect
       (seq-every-p
        #'identity
        ;; Loop over functions
        (mapcar (lambda (fun)
                  ;; Work on sequence of argument strings for each function
                  (thread-last (gethash fun ht-given)
                    (mapcar (lambda (str) (string-match " | " str)))
                    ;; should be a list with non-nil elements only
                    (seq-every-p #'identity)))
                fun-names-lcdf))
       :to-be-truthy))
    ;;
    (it "uses | for all *_lccdf arguments when given is true"
      (expect
       (seq-every-p
        #'identity
        ;; Loop over functions
        (mapcar (lambda (fun)
                  ;; Work on sequence of argument strings for each function
                  (thread-last (gethash fun ht-given)
                    (mapcar (lambda (str) (string-match " | " str)))
                    ;; should be a list with non-nil elements only
                    (seq-every-p #'identity)))
                fun-names-lccdf))
       :to-be-truthy))
    ;;
    (it "does not use | for other function arguments even when given is true"
      (expect
       (seq-every-p
        #'identity
        ;; Loop over functions
        (mapcar (lambda (fun)
                  ;; Work on sequence of argument strings for each function
                  (thread-last (gethash fun ht-given)
                    (mapcar (lambda (str) (string-match " | " str)))
                    ;; should be a list with non-nil elements only
                    (seq-every-p #'null)))
                fun-names-others))
       :to-be-truthy))
    ;;
    (it "does not use | for all *_lpdf arguments when given is false"
      (expect
       (seq-every-p
        #'identity
        ;; Loop over functions. Should return (t t ... t)
        (mapcar (lambda (fun)
                  ;; Work on sequence of argument strings for each function
                  (thread-last (gethash fun ht)
                    (mapcar (lambda (str) (string-match " | " str)))
                    ;; Should be a list with nil elements only
                    (seq-every-p #'null)))
                fun-names-lpdf))
       :to-be-truthy))
    ;;
    (it "does not use | for all *_lpmf arguments when given is false"
      (expect
       (seq-every-p
        #'identity
        ;; Loop over functions. Should return (t t ... t)
        (mapcar (lambda (fun)
                  ;; Work on sequence of argument strings for each function
                  (thread-last (gethash fun ht)
                    (mapcar (lambda (str) (string-match " | " str)))
                    ;; Should be a list with nil elements only
                    (seq-every-p #'null)))
                fun-names-lpmf))
       :to-be-truthy))
    ;;
    (it "does not use | for all *_lcdf arguments when given is false"
      (expect
       (seq-every-p
        #'identity
        ;; Loop over functions. Should return (t t ... t)
        (mapcar (lambda (fun)
                  ;; Work on sequence of argument strings for each function
                  (thread-last (gethash fun ht)
                    (mapcar (lambda (str) (string-match " | " str)))
                    ;; Should be a list with nil elements only
                    (seq-every-p #'null)))
                fun-names-lcdf))
       :to-be-truthy))
    ;;
    (it "does not use | for all *_lccdf arguments when given is false"
      (expect
       (seq-every-p
        #'identity
        ;; Loop over functions. Should return (t t ... t)
        (mapcar (lambda (fun)
                  ;; Work on sequence of argument strings for each function
                  (thread-last (gethash fun ht)
                    (mapcar (lambda (str) (string-match " | " str)))
                    ;; Should be a list with nil elements only
                    (seq-every-p #'null)))
                fun-names-lccdf))
       :to-be-truthy))
    ;;
    (it "does not use | for other function arguments when given is false"
      (expect
       (seq-every-p
        #'identity
        ;; Loop over functions
        (mapcar (lambda (fun)
                  ;; Work on sequence of argument strings for each function
                  (thread-last (gethash fun ht)
                    (mapcar (lambda (str) (string-match " | " str)))
                    ;; should be a list with non-nil elements only
                    (seq-every-p #'null)))
                fun-names-others))
       :to-be-truthy))))


(provide 'test-eldoc-stan-create-json)
;;; test-eldoc-stan-create-json.el ends here
