;;; test-c++-mode.el --- Tests for c++-mode with stan-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Kazuki Yoshida

;; Author: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; URL: https://github.com/stan-dev/stan-mode/tree/master/stan-mode
;; Keywords: languages
;; Version: 10.1.0
;; Created: 2021-01-29
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
(require 'cc-mode)
(require 'stan-mode)

;;; c++-mode check
;; stan-mode may interfere with c++-mode syntax highlighting
;; https://github.com/stan-dev/stan-mode/issues/64
(defun test-c++--highlight (src)
  "Syntax hightlight SRC with `c++-mode'.

Adopted from test/groovy-unit-test.el in the `groovy-mode'."
  (with-temp-buffer
    (insert src)
    (goto-char (point-min))
    ;; Major mode for editing C++ code.
    (c++-mode)
    ;; Ensure we've syntax-highlighted the whole buffer.
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings
        (font-lock-fontify-buffer)))
    (buffer-string)))

(defun test-c++--check-face (regexp src-highlighted &optional start)
  "Return the face of REGEXP in SRC-HIGHLIGHTED.

START is the optional start position of search."
  ;;
  (let ((pos (string-match regexp src-highlighted start)))
    (if pos
        (get-char-property pos
                           'face
                           src-highlighted)
      (error "String not found! Thus, the face is undefined!"))))

(describe "c++-mode font lock (WITHOUT stan-mode loaded)"
  (let* ((src-highlighted
          (test-c++--highlight
           ;; Taken from:
           ;; https://github.com/stan-dev/stan/blob/develop/src/stan/mcmc/hmc/nuts/adapt_unit_e_nuts.hpp
           "#ifndef STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP
#define STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP

#include <stan/callbacks/logger.hpp>
#include <stan/mcmc/hmc/nuts/unit_e_nuts.hpp>
#include <stan/mcmc/stepsize_adapter.hpp>

namespace stan {
namespace mcmc {
/**
 * The No-U-Turn sampler (NUTS) with multinomial sampling
 * with a Gaussian-Euclidean disintegration and unit metric
 * and adaptive step size
 */
template <class Model, class BaseRNG>
class adapt_unit_e_nuts : public unit_e_nuts<Model, BaseRNG>,
                          public stepsize_adapter {
 public:
  adapt_unit_e_nuts(const Model& model, BaseRNG& rng)
      : unit_e_nuts<Model, BaseRNG>(model, rng) {}

  ~adapt_unit_e_nuts() {}

  sample transition(sample& init_sample, callbacks::logger& logger) {
    sample s = unit_e_nuts<Model, BaseRNG>::transition(init_sample, logger);

    if (this->adapt_flag_)
      this->stepsize_adaptation_.learn_stepsize(this->nom_epsilon_,
                                                s.accept_stat());

    return s;
  }

  void disengage_adaptation() {
    base_adapter::disengage_adaptation();
    this->stepsize_adaptation_.complete_adaptation(this->nom_epsilon_);
  }
};

}  // namespace mcmc
}  // namespace stan
#endif")))
    ;;
    (describe "gives #ifndef font-lock-preprocessor-face and handle its body"
      (it "#ifndef STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP"
        (expect
         (test-c++--check-face (rx "#ifndef STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP") src-highlighted)
         :to-be
         'font-lock-preprocessor-face))
      ;; Leave spaces to align
      (it "        STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP"
        (expect
         (test-c++--check-face (rx "STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP") src-highlighted)
         :to-be
         nil)))
    ;;
    (describe "gives #define font-lock-preprocessor-face and handle its body"
      (it "#define STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP"
        (expect
         (test-c++--check-face (rx "#define STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP") src-highlighted)
         :to-be
         'font-lock-preprocessor-face))
      (it "        STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP"
        (expect
         (test-c++--check-face (rx "STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP

#include <stan/callbacks/logger.hpp>") src-highlighted)
         :to-be
         'font-lock-variable-name-face)))
    ;;
    (describe "gives #include font-lock-preprocessor-face and handle its body"
      (it "#include <stan/callbacks/logger.hpp>"
        (expect
         (test-c++--check-face (rx "#include <stan/callbacks/logger.hpp>") src-highlighted)
         :to-be
         'font-lock-preprocessor-face))
      (it "         <stan/callbacks/logger.hpp>"
        (expect
         (test-c++--check-face (rx "<stan/callbacks/logger.hpp>") src-highlighted)
         :to-be
         'font-lock-string-face))
      (it "          stan/callbacks/logger.hpp>"
        (expect
         (test-c++--check-face (rx "stan/callbacks/logger.hpp>") src-highlighted)
         :to-be
         'font-lock-string-face)))
    ;;
    (describe "gives namespace correct faces"
      (it "namespace stan {"
        (expect
         (test-c++--check-face (rx "namespace stan {") src-highlighted)
         :to-be
         'font-lock-keyword-face))
      (it "          stan {"
        (expect
         (test-c++--check-face (rx "stan {") src-highlighted)
         :to-be
         'font-lock-constant-face))
      (it "               {"
        (expect
         (test-c++--check-face (rx "{") src-highlighted)
         :to-be
         nil)))
    ;;
    (describe "gives documentation part correct faces"
      (it "/**"
        (expect
         (test-c++--check-face (rx "/**") src-highlighted)
         :to-be
         'font-lock-doc-face))
      (it " **"
        (expect
         (test-c++--check-face (rx "**") src-highlighted)
         :to-be
         'font-lock-doc-face))
      (it " * The No-U-Turn sampler (NUTS) with multinomial sampling"
        (expect
         (test-c++--check-face (rx "* The No-U-Turn sampler (NUTS) with multinomial sampling") src-highlighted)
         :to-be
         'font-lock-doc-face))
      (it "   The No-U-Turn sampler (NUTS) with multinomial sampling"
        (expect
         (test-c++--check-face (rx "The No-U-Turn sampler (NUTS) with multinomial sampling") src-highlighted)
         :to-be
         'font-lock-doc-face)))
    ;;
    (describe "gives template correct faces"
      (it "template <class Model, class BaseRNG>"
        (expect
         (test-c++--check-face (rx "template <class Model, class BaseRNG>") src-highlighted)
         :to-be
         'font-lock-keyword-face))
      (it "         <class Model, class BaseRNG>"
        (expect
         (test-c++--check-face (rx "<class Model, class BaseRNG>") src-highlighted)
         :to-be
         nil))
      (it "          class Model, class BaseRNG>"
        (expect
         (test-c++--check-face (rx "class Model, class BaseRNG>") src-highlighted)
         :to-be
         'font-lock-keyword-face))
      (it "                Model, class BaseRNG>"
        (expect
         (test-c++--check-face (rx "Model, class BaseRNG") src-highlighted)
         :to-be
         'font-lock-type-face)))
    ;;
    (describe "gives class correct faces"
      (it "class adapt_unit_e_nuts : public unit_e_nuts<Model, BaseRNG>,"
        (expect
         (test-c++--check-face (rx "class adapt_unit_e_nuts : public unit_e_nuts<Model, BaseRNG>,") src-highlighted)
         :to-be
         'font-lock-keyword-face))
      (it "      adapt_unit_e_nuts : public unit_e_nuts<Model, BaseRNG>,"
        (expect
         (test-c++--check-face (rx "adapt_unit_e_nuts : public unit_e_nuts<Model, BaseRNG>,") src-highlighted)
         :to-be
         'font-lock-type-face)))
    ;;
    (describe "gives comment correct faces"
      (it "   // namespace mcmc"
        (expect
         (test-c++--check-face (rx "// namespace mcmc") src-highlighted)
         :to-be
         'font-lock-comment-delimiter-face))
      (it "      namespace mcmc"
        (expect
         (test-c++--check-face (rx "namespace mcmc
}  // namespace stan") src-highlighted)
         :to-be
         'font-lock-comment-face)))
    ;;
    (describe "gives #endif correct faces"
      (it "#endif"
        (expect
         (test-c++--check-face (rx "#endif") src-highlighted)
         :to-be
         'font-lock-preprocessor-face))
      (it " endif"
        (expect
         (test-c++--check-face (rx "endif") src-highlighted)
         :to-be
         'font-lock-preprocessor-face)))))

(describe "c++-mode font lock (AFTER stan-mode loaded elsewhere)"
  ;; Load stan-mode elsewhere
  (with-temp-buffer
    ;; This will invoke `stan-advice-add-c-syntactic-end-of-macro'.
    (stan-mode))
  ;;
  (let* ((src-highlighted
          (test-c++--highlight
           ;; Taken from:
           ;; https://github.com/stan-dev/stan/blob/develop/src/stan/mcmc/hmc/nuts/adapt_unit_e_nuts.hpp
           "#ifndef STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP
#define STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP

#include <stan/callbacks/logger.hpp>
#include <stan/mcmc/hmc/nuts/unit_e_nuts.hpp>
#include <stan/mcmc/stepsize_adapter.hpp>

namespace stan {
namespace mcmc {
/**
 * The No-U-Turn sampler (NUTS) with multinomial sampling
 * with a Gaussian-Euclidean disintegration and unit metric
 * and adaptive step size
 */
template <class Model, class BaseRNG>
class adapt_unit_e_nuts : public unit_e_nuts<Model, BaseRNG>,
                          public stepsize_adapter {
 public:
  adapt_unit_e_nuts(const Model& model, BaseRNG& rng)
      : unit_e_nuts<Model, BaseRNG>(model, rng) {}

  ~adapt_unit_e_nuts() {}

  sample transition(sample& init_sample, callbacks::logger& logger) {
    sample s = unit_e_nuts<Model, BaseRNG>::transition(init_sample, logger);

    if (this->adapt_flag_)
      this->stepsize_adaptation_.learn_stepsize(this->nom_epsilon_,
                                                s.accept_stat());

    return s;
  }

  void disengage_adaptation() {
    base_adapter::disengage_adaptation();
    this->stepsize_adaptation_.complete_adaptation(this->nom_epsilon_);
  }
};

}  // namespace mcmc
}  // namespace stan
#endif")))
    ;;
    (describe "gives #ifndef font-lock-preprocessor-face and handle its body"
      (it "#ifndef STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP"
        (expect
         (test-c++--check-face (rx "#ifndef STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP") src-highlighted)
         :to-be
         'font-lock-preprocessor-face))
      ;; Leave spaces to align
      (it "        STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP"
        (expect
         (test-c++--check-face (rx "STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP") src-highlighted)
         :to-be
         nil)))
    ;;
    (describe "gives #define font-lock-preprocessor-face and handle its body"
      (it "#define STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP"
        (expect
         (test-c++--check-face (rx "#define STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP") src-highlighted)
         :to-be
         'font-lock-preprocessor-face))
      (it "        STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP"
        (expect
         (test-c++--check-face (rx "STAN_MCMC_HMC_NUTS_ADAPT_UNIT_E_NUTS_HPP

#include <stan/callbacks/logger.hpp>") src-highlighted)
         :to-be
         'font-lock-variable-name-face)))
    ;;
    (describe "gives #include font-lock-preprocessor-face and handle its body"
      (it "#include <stan/callbacks/logger.hpp>"
        (expect
         (test-c++--check-face (rx "#include <stan/callbacks/logger.hpp>") src-highlighted)
         :to-be
         'font-lock-preprocessor-face))
      (it "         <stan/callbacks/logger.hpp>"
        (expect
         (test-c++--check-face (rx "<stan/callbacks/logger.hpp>") src-highlighted)
         :to-be
         'font-lock-string-face))
      (it "          stan/callbacks/logger.hpp>"
        (expect
         (test-c++--check-face (rx "stan/callbacks/logger.hpp>") src-highlighted)
         :to-be
         'font-lock-string-face)))
    ;;
    (describe "gives namespace correct faces"
      (it "namespace stan {"
        (expect
         (test-c++--check-face (rx "namespace stan {") src-highlighted)
         :to-be
         'font-lock-keyword-face))
      (it "          stan {"
        (expect
         (test-c++--check-face (rx "stan {") src-highlighted)
         :to-be
         'font-lock-constant-face))
      (it "               {"
        (expect
         (test-c++--check-face (rx "{") src-highlighted)
         :to-be
         nil)))
    ;;
    (describe "gives documentation part correct faces"
      (it "/**"
        (expect
         (test-c++--check-face (rx "/**") src-highlighted)
         :to-be
         'font-lock-doc-face))
      (it " **"
        (expect
         (test-c++--check-face (rx "**") src-highlighted)
         :to-be
         'font-lock-doc-face))
      (it " * The No-U-Turn sampler (NUTS) with multinomial sampling"
        (expect
         (test-c++--check-face (rx "* The No-U-Turn sampler (NUTS) with multinomial sampling") src-highlighted)
         :to-be
         'font-lock-doc-face))
      (it "   The No-U-Turn sampler (NUTS) with multinomial sampling"
        (expect
         (test-c++--check-face (rx "The No-U-Turn sampler (NUTS) with multinomial sampling") src-highlighted)
         :to-be
         'font-lock-doc-face)))
    ;;
    (describe "gives template correct faces"
      (it "template <class Model, class BaseRNG>"
        (expect
         (test-c++--check-face (rx "template <class Model, class BaseRNG>") src-highlighted)
         :to-be
         'font-lock-keyword-face))
      (it "         <class Model, class BaseRNG>"
        (expect
         (test-c++--check-face (rx "<class Model, class BaseRNG>") src-highlighted)
         :to-be
         nil))
      (it "          class Model, class BaseRNG>"
        (expect
         (test-c++--check-face (rx "class Model, class BaseRNG>") src-highlighted)
         :to-be
         'font-lock-keyword-face))
      (it "                Model, class BaseRNG>"
        (expect
         (test-c++--check-face (rx "Model, class BaseRNG") src-highlighted)
         :to-be
         'font-lock-type-face)))
    ;;
    (describe "gives class correct faces"
      (it "class adapt_unit_e_nuts : public unit_e_nuts<Model, BaseRNG>,"
        (expect
         (test-c++--check-face (rx "class adapt_unit_e_nuts : public unit_e_nuts<Model, BaseRNG>,") src-highlighted)
         :to-be
         'font-lock-keyword-face))
      (it "      adapt_unit_e_nuts : public unit_e_nuts<Model, BaseRNG>,"
        (expect
         (test-c++--check-face (rx "adapt_unit_e_nuts : public unit_e_nuts<Model, BaseRNG>,") src-highlighted)
         :to-be
         'font-lock-type-face)))
    ;;
    (describe "gives comment correct faces"
      (it "   // namespace mcmc"
        (expect
         (test-c++--check-face (rx "// namespace mcmc") src-highlighted)
         :to-be
         'font-lock-comment-delimiter-face))
      (it "      namespace mcmc"
        (expect
         (test-c++--check-face (rx "namespace mcmc
}  // namespace stan") src-highlighted)
         :to-be
         'font-lock-comment-face)))
    ;;
    (describe "gives #endif correct faces"
      (it "#endif"
        (expect
         (test-c++--check-face (rx "#endif") src-highlighted)
         :to-be
         'font-lock-preprocessor-face))
      (it " endif"
        (expect
         (test-c++--check-face (rx "endif") src-highlighted)
         :to-be
         'font-lock-preprocessor-face)))))

(provide 'test-c++-mode)
;;; test-c++-mode.el ends here
