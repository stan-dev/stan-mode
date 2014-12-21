[![MELPA](http://melpa.org/packages/stan-mode-badge.svg)](http://melpa.org/#/stan-mode)
[![MELPA Stable](http://stable.melpa.org/packages/stan-mode-badge.svg)](http://stable.melpa.org/#/stan-mode)

# Emacs support for Stan

This repository contains several Emacs packages to make editing [Stan](https://code.google.com/p/stan/) files easier.

- `stan-mode` is a major mode for editing Stan files.
  Its current features include:

  - syntax highlighting
  - indentation
  - [Compilation Mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation-Mode.html) support if the stan file is compiled using `CmdStan`.
  - [imenu](http://www.emacswiki.org/emacs/ImenuMode) support for blocks, variables, and user-defined functions.

- `flycheck-stan`: Adds Stan support for [https://github.com/flycheck/flycheck](flycheck). Flycheck is an on-the-fly syntax checker.
- `stan-snippets`: Adds Stan support for [yasnippet](https://github.com/capitaomorte/yasnippet). Yasnippet is a teplate system for Emacs. Snippets are defined for blocks, control structures, and *all* the built-in functions and distributions.
- `ac-stan`: Add Stan support for [autocomplete-mode](http://cx4a.org/software/auto-complete/).

## Installing

The recommended way to install `stan-mode` is using the built-in package manager of Emacs 24, `package.el`.
This allows for easy updating of `stan-mode` from within emacs.
For more information on `package.el`, see the [EmacsWiki](http://emacswiki.org/emacs/ELPA)

The stan packages are available from [MELPA](http://melpa.org) or [MELPA stable](http://stable.melpa.org).
If you're not already using MELPA, follow its installation [instructions](http://melpa.org/#/getting-started).

You can install `stan-mode` with the following commands:

<kbd>M-x package-install [RET] stan-mode [RET]</kbd>
<kbd>M-x package-install [RET] flycheck-stan [RET]</kbd>
<kbd>M-x package-install [RET] stan-snippets [RET]</kbd>
<kbd>M-x package-install [RET] ac-stan [RET]</kbd>

## Usage

### stan-mode

To use add the following to your `init.el` file:
```elisp
(require 'stan-mode)
```

### stan-snippets

To use add the following to your `init.el` file:
```elisp
(require 'stan-snippets)
```

### ac-stan

To use it, add the following add the following to your `init.el`:
```elisp
(require 'ac-stan)
```

### flycheck-stan

To use `flycheck-stan`, you need to install [CmdStan](http://mc-stan.org/cmdstan.html).
`flycheck-stan` uses the `stanc` binary to check the syntax, so it must either be in the `PATH`, or you need to set `stan-stanc-bin` to the path to `stanc`.

To use it, add the following add the following to your `init.el`:
```elisp
(require 'flycheck-stan)
(add-hook 'stan-mode-hook 'flycheck-mode)
```

## Developers

This may be of use to developers which would like to support the Stan modeling language in other editors or applications.
The file `stan-lang/stan_lang.json` contains the keywords and all the signatures and documentation of all the functions of Stan.

## License

`*.el` files are free software under the [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html).

The file `stan-lang/stan-lang.json` is available under the Public Domain Dedication and License v1.0 whose full text can be found at: http://www.opendatacommons.org/licenses/pddl/1.0/ - See more at: http://opendatacommons.org/licenses/pddl/#sthash.UJfFWezm.dpuf

<!--  LocalWords:  stan imenu yasnippet flymake MELPA kbd RET init '
 -->
<!--  LocalWords:  mapc EmacsWiki cd 'load 'stan 'flymake Aquamacs 
 -->
<!--  LocalWords:  GPL stanc ' 'load 'stan autocomplete setq 'flymake
 -->
<!--  LocalWords:  lang json el emacs
 -->
