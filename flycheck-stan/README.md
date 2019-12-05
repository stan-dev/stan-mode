# flycheck syntax checker for Stan

[![MELPA](http://melpa.org/packages/flycheck-stan-badge.svg)](http://melpa.org/#/flycheck-stan)
[![MELPA Stable](http://stable.melpa.org/packages/flycheck-stan-badge.svg)](http://stable.melpa.org/#/flycheck-stan)

![Example flycheck](example_flycheck.png)

`flycheck-stan` adds a [`flycheck`](https://www.flycheck.org/en/latest/) syntax checker for [Stan](https://mc-stan.org) program.


## Installation
### Emacs package
Install this package using the built-in package manager: `package.el`. `flycheck-stan` is available from [MELPA](http://melpa.org). If you're not already using MELPA, follow its [installation instructions](http://melpa.org/#/getting-started).

You can then install it using the following command:

<kbd>M-x package-install [RET] flycheck-stan [RET]</kbd>

### stanc
This package requires an external software, the `stanc` binary, which is a part of the [`CmdStan`](https://github.com/stan-dev/cmdstan). `stanc` translates a stan file into a c++ file which can then be compiled into an executable.

`CmdStan` has to be installed from the source code as of December 2019. Please follow the instructions in the latest [release](https://github.com/stan-dev/cmdstan/releases). See [Getting Started with CmdStan](https://github.com/stan-dev/cmdstan/wiki/Getting-Started-with-CmdStan) if cloning. It proceed as follows, but the directory names are specific to your environment.

```{shell}
## Move to an appropriate directory for source code download
cd ~/source_code/
## Clone the source code repo (Skip if downloading a release)
git clone https://github.com/stan-dev/cmdstan.git --recursive
## Move in to the new directory
cd cmdstan
## Build all tools
make build
## Check version (developmental version may just give a commit)
./bin/stanc --version
## stanc3 should show options like --debug-lex, --debug-parse, etc.
./bin/stanc --help
## Make stanc accessible via a symbolic link
## If using stanc version 2
ln -s ./bin/stanc /usr/local/bin/stanc2
## If using stanc version 3
ln -s ./bin/stanc /usr/local/bin/stanc3
```

Please note there are two different implementation of `stanc`, Version 2 and Version 3, with different behaviors. As they require different handling, `flycheck-stan` by default expects them to be named accordingly. If you do not want to follow this convention, see the additional configuration below.


## Configuration
An example configuration using the [`use-package`](https://github.com/jwiegley/use-package) macro is the following.

```{lisp}
;; flycheck
(use-package flycheck
  :config
  ;; Uncomment the line below if you want to enable it everywhere.
  ;;(global-flycheck-mode +1)
  )

;; flycheck-stan
(use-package flycheck-stan
  ;; Add a hook to setup `flycheck-stan' upon `stan-mode' entry
  :hook ((stan-mode . flycheck-stan-stanc2-setup)
         (stan-mode . flycheck-stan-stanc3-setup))
  :config
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc2'
  (setq flycheck-stanc-executable nil)
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc3'
  (setq flycheck-stanc3-executable nil))
```

It can also be written as follows.

```{lisp}
;; flycheck
(require 'flycheck)
(global-flycheck-mode +1)

;; flycheck-stan
(require 'flycheck-stan)
;; Add a hook to setup `flycheck-stan' upon `stan-mode' entry
(add-hook 'stan-mode-hook flycheck-stan-stanc2-setup)
(add-hook 'stan-mode-hook flycheck-stan-stanc3-setup)
;; A string containing the name or the path of the stanc2 executable
;; If nil, defaults to `stanc2'
(setq flycheck-stanc-executable nil)
;; A string containing the name or the path of the stanc2 executable
;; If nil, defaults to `stanc3'
(setq flycheck-stanc3-executable nil)
```


## flycheck installation verification

After the initial installation, use [M-x flycheck-verify-setup](https://www.flycheck.org/en/latest/user/troubleshooting.html#verify-your-setup) to check the configuration.

For macOS, see also [Flycheck can’t find any programs in GUI Emacs on MacOS](https://www.flycheck.org/en/latest/user/troubleshooting.html#flycheck-cant-find-any-programs-in-gui-emacs-on-macos).


## flycheck interaction

See [Flycheck Quickstart](https://www.flycheck.org/en/latest/user/quickstart.html).

The syntax check should be automatic. The following commands may come in handy.

| flycheck command            | Action                                               |
|-----------------------------|------------------------------------------------------|
| `M-x flycheck-verify-setup` | Check whether the checker(s) for current buffer.     |
| `M-x flycheck-compile`      | Run a checker on the current stan program. |


## Inner workings
`flycheck-stan-stanc2/3-setup` will add the `stanc`/`stanc3` checker to the global value of the `flycheck-checkers` list. Note this checker is only active in a `stan-mode` buffer.

The most important part of the checker definition is creating the pattern matchers. `flycheck` uses extended `rx` notations for regular expression. Being able to use this notation directly in the `M-x re-builder` is nice. For this purpose, a function named `flycheck-stan-enhance-rx-buffer-locally` is included.

The naming convention `stanc2` and `stanc3` is currently necessary to test the package with both implementations of `stanc`.

References:

- [Flycheck Developer’s Guide](https://www.flycheck.org/en/latest/developer/developing.html)
