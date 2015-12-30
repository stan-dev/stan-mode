# Emacs support for Stan

[![Build Status](https://travis-ci.org/stan-dev/stan-mode.svg?branch=master)](https://travis-ci.org/stan-dev/stan-mode)

This repository contains several Emacs packages and tools to make editing [Stan](https://code.google.com/p/stan/) files easier.

- `stan-mode` is a major mode for editing Stan files.
   Its current features include:

  - syntax highlighting
  - indentation
  - [imenu](http://www.emacswiki.org/emacs/ImenuMode) support for blocks, variables, and user-defined functions.

- `stan-snippets`: Adds Stan support for [yasnippet](https://github.com/capitaomorte/yasnippet). Yasnippet is a template system for Emacs. Snippets are defined for blocks, control structures, and *all* the built-in functions and distributions.
- `indent-stan-files`: A shell script that uses `stan-mode` to indent a file. See its [README](https://github.com/stan-dev/stan-mode/blob/develop/indent-stan-files/README.md).
- `stan-lang`: The file `stan_lang.json` contains all keywords, functions (with their signatures and documentation) in the Stan modeling language. This is used to generate the keyword lists and snippets used by the modes. It could also be useful for developers designing tools for Stan, e.g. other editor modes.

## Installing

### package.el

The recommended way to install these packages is using the built-in package manager: `package.el`.
These packages are available from [MELPA](http://melpa.org).
If you're not already using MELPA, follow its installation [instructions](http://melpa.org/#/getting-started).

You can then install the packages using the following commands:

<kbd>M-x package-install [RET] stan-mode [RET]</kbd>

<kbd>M-x package-install [RET] stan-snippets [RET]</kbd>

<!-- <kbd>M-x package-install [RET] ac-stan [RET]</kbd> -->

If the installation does not work, try refreshing the package list:

<kbd>M-x package-refresh-contents [RET]</kbd>

Or add the following to you `init.el`:
```lisp
(package-refresh-contents)
(mapc
 (lambda (p)
   (unless (package-installed-p p)
     (package-install p)))
 '(stan-mode stan-snippets))
```

### Cask

Another way to manage dependencies is to to use [Cask](https://github.com/cask/cask).
See its [docs](http://cask.readthedocs.org/en/latest/guide/introduction.html#emacs-configuration) for an argument as to why to use Cask to manage your configuration.

Simply add the following to your Cask file:
```lisp
(source melpa)
(depends-on "stan-mode")
(depends-on "stan-snippets")
```
and from the command line in the same directory as the Cask file use `cask` to install the packages,
```console
$ cask install
```
See the Cask [documentation](http://cask.readthedocs.org/en/latest/index.html) for more information.

## stan-mode

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-blue.svg)][COPYING]
[![MELPA](http://melpa.org/packages/stan-mode-badge.svg)](http://melpa.org/#/stan-mode)
[![MELPA Stable](http://stable.melpa.org/packages/stan-mode-badge.svg)](http://stable.melpa.org/#/stan-mode)

To use, add the following to your `init.el` file:
```lisp
(require 'stan-mode)
```

## stan-snippets

[![MELPA](http://melpa.org/packages/stan-snippets-badge.svg)](http://melpa.org/#/stan-snippets)
[![MELPA Stable](http://stable.melpa.org/packages/stan-snippets-badge.svg)](http://stable.melpa.org/#/stan-snippets)

To use, add the following to your `init.el` file:
```lisp
(require 'stan-snippets)
```
To use `yasnippet` globally:
```lisp
(yas-global-mode 1)
```
Else, to use `yasnippet` only for `stan-mode`:
```lisp
(add-hook 'stan-mode-hook '(lambda () (yas-minor-mode)))
```

See the documenation for [yasnippet](https://github.com/capitaomorte/yasnippet) for more information on using `yasnippet-mode`.

<!-- ## ac-stan -->

<!-- To use, add the following add the following to your `init.el`: -->
<!-- ```lisp -->
<!-- (require 'ac-stan) -->
<!-- ``` -->
<!-- To use `auto-complete` mode, -->
<!-- ```lisp -->
<!-- (require 'auto-complete-config) -->
<!-- (ac-config-default) -->
<!-- ``` -->
<!-- See the Auto Complete Mode [documentation](http://cx4a.org/software/auto-complete/manual.html) for more information on using `autocomplete-mode`. -->

## Auto Complete mode

`stan-mode` does not directly support [autocomplete](http://cx4a.org/software/auto-complete/).
However a dictionary compatible with autocomplete-mode is available for stan-mode.
To use autcomplete with stan, download the [stan-mode](https://raw.githubusercontent.com/stan-dev/stan-mode/master/ac-stan/ac-dict/stan-mode), and follow the autocomplete directions for using a [major-mode dictionary](http://auto-complete.org/doc/manual.html#major-mode-dictionary-and-extension-dictionary).

## Updating packages

To update stan-mode when a  version of the Stan language comes out:

1. Replace `stan-lang/stan-functions-*.txt` with the newest version from CmdStan.
2. Build the emacs files
``` shell
$ make
```
3. Save and commit the changes
4. Bump the version number of the emacs packages. For example, to bump to 8.0.0. This is different than the Stan language version.

``` shell
$ ./update-versions.sh 8.0.0
```
5. Tag the commit and push the tag

``` shell
$ git tag v8.0.0
$ git push --tags
```



## License

All packages are free software under the [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html).

<!--  LocalWords:  stan imenu yasnippet flymake MELPA kbd RET init '
 -->
<!--  LocalWords:  mapc EmacsWiki cd 'load 'stan 'flymake Aquamacs 
 -->
<!--  LocalWords:  GPL stanc ' 'load 'stan autocomplete setq 'flymake
 -->
<!--  LocalWords:  lang json el emacs CmdStan flycheck 'stan v3
 -->
<!--  LocalWords:  'ac 'flycheck v1
 -->
