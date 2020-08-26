# auto-complete support for Stan

![Example auto-completion](example_auto_completion.png)

`ac-stan` adds a dictionary for the [`auto-complete`](https://github.com/auto-complete/auto-complete) mode covering [Stan](https://mc-stan.org) function names. Please note that the `auto-complete` mode has been archived and not under active development.


## Installation

This is not on MELPA because `auto-complete` itself is semi-deprecated. Please clone this repo or otherwise copy the `ac-stan` folder to your local system.


## Configuration
An example configuration using the [`use-package`](https://github.com/jwiegley/use-package) macro is the following.

```{lisp}
(use-package ac-stan
  :load-path "path-to-your-directory/ac-stan/"
  ;; Add a hook to setup `ac-stan' upon `stan-mode' entry
  :hook (stan-mode . ac-stan-ac-mode-setup))
```

It can also be written as follows.

```{lisp}
(require 'ac-stan)
;; Add a hook to setup `ac-stan' upon `stan-mode' entry
(add-hook 'stan-mode-hook 'ac-stan-ac-mode-setup)
```

## Inner workings
The `ac-dict/stan-mode` file is the dictionary file containing keywords in Stan.
