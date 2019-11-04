;;; test-stan-snippets.el --- A buttercup test suite for stan-snippets -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kazuki Yoshida

;; Author: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>,
;; Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>,
;; URL: https://github.com/stan-dev/stan-mode/tree/master/stan-snippets
;; Keywords: languages,tools
;; Version: 10.0.0
;; Created: 2019-07-26
;; Package-Requires: ((emacs "24.3") (stan-mode "10.0.0") (yasnippet "0.8.0"))


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
(require 'seq)
(require 'stan-snippets)


(describe "stan-snippets-initialize"
  (it "works on yas-snippet-dirs without stan-snippets directory"
    (expect
     (seq-empty-p
      (seq-filter (lambda (dir)
                    (string-match "stan-snippets" dir))
                  yas-snippet-dirs))
     :to-be-truthy))
  ;;
  (it "updates yas-snippet-dirs with stan-snippets directory"
    (stan-snippets-initialize)
    (expect
     (seq-empty-p
      (seq-filter (lambda (dir)
                    (string-match "stan-snippets" dir))
                  yas-snippet-dirs))
     :not :to-be-truthy)))


(describe "yas-expand in stan-mode"
  ;; `yas-expand' corresponds to TAB to expand initially.
  ;;
  (before-all
    (stan-snippets-initialize))
  ;;
  (it "expands normal_lpdf correctly"
    (expect
     (with-temp-buffer
       (stan-mode)
       (yas-minor-mode 1)
       (insert "normal_lpdf")
       (yas-expand)
       (buffer-substring-no-properties (point-min) (point-max)))
     :to-equal
     "normal_lpdf(y | mu, sigma)"))
  ;;
  (it "expands normal correctly"
    (expect
     (with-temp-buffer
       (stan-mode)
       (yas-minor-mode 1)
       (insert "normal")
       (yas-expand)
       (buffer-substring-no-properties (point-min) (point-max)))
     :to-equal
     "normal(mu, sigma)"))
  ;;
  (it "expands binomial_lmf correctly"
    (expect
     (with-temp-buffer
       (stan-mode)
       (yas-minor-mode 1)
       (insert "binomial_lpmf")
       (yas-expand)
       (buffer-substring-no-properties (point-min) (point-max)))
     :to-equal
     "binomial_lpmf(n | N, theta)"))
  ;;
  (it "expands binomial correctly"
    (expect
     (with-temp-buffer
       (stan-mode)
       (yas-minor-mode 1)
       (insert "binomial")
       (yas-expand)
       (buffer-substring-no-properties (point-min) (point-max)))
     :to-equal
     "binomial(N, theta)")))


(describe "yas-expand in stan-mode (with argument entries)"
  ;; See
  ;; https://github.com/joaotavora/yasnippet/blob/master/yasnippet-tests.el
  ;; `yas-expand' corresponds to TAB to expand initially.
  ;; `yas-next-field-or-maybe-expand' corresponds to TAB to move to
  ;; next field.
  ;;
  (before-all
    (stan-snippets-initialize))
  ;;
  (it "expands normal_lpdf correctly"
    (expect
     (with-temp-buffer
       (stan-mode)
       (yas-minor-mode 1)
       (insert "normal_lpdf")
       (yas-expand)
       (insert "outcome")
       (yas-next-field-or-maybe-expand)
       (insert "mean")
       (yas-next-field-or-maybe-expand)
       (insert "sd")
       (yas-next-field-or-maybe-expand)
       (insert ";")
       (buffer-substring-no-properties (point-min) (point-max)))
     :to-equal
     "normal_lpdf(outcome | mean, sd);"))
  ;;
  (it "expands normal correctly"
    (expect
     (with-temp-buffer
       (stan-mode)
       (yas-minor-mode 1)
       (insert "outcome ~ normal")
       (yas-expand)
       (insert "mean")
       (yas-next-field-or-maybe-expand)
       (insert "sd")
       (yas-next-field-or-maybe-expand)
       (insert ";")
       (buffer-substring-no-properties (point-min) (point-max)))
     :to-equal
     "outcome ~ normal(mean, sd);"))
  ;;
  (it "expands binomial_lmf correctly"
    (expect
     (with-temp-buffer
       (stan-mode)
       (yas-minor-mode 1)
       (insert "binomial_lpmf")
       (yas-expand)
       (insert "count")
       (yas-next-field-or-maybe-expand)
       (insert "total")
       (yas-next-field-or-maybe-expand)
       (insert "prob")
       (yas-next-field-or-maybe-expand)
       (insert ";")
       (buffer-substring-no-properties (point-min) (point-max)))
     :to-equal
     "binomial_lpmf(count | total, prob);"))
  ;;
  (it "expands binomial correctly"
    (expect
     (with-temp-buffer
       (stan-mode)
       (yas-minor-mode 1)
       (insert "count ~ binomial")
       (yas-expand)
       (insert "total")
       (yas-next-field-or-maybe-expand)
       (insert "prob")
       (yas-next-field-or-maybe-expand)
       (insert ";")
       (buffer-substring-no-properties (point-min) (point-max)))
     :to-equal
     "count ~ binomial(total, prob);")))



(provide 'test-stan-snippets)
;;; test-stan-snippets.el ends here
