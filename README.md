# Emacs support for Stan

This repository contains several packages providing support for editing [Stan](https://code.google.com/p/stan/) in Emacs.

`stan-mode` is the primary package; it is a major mode for editing Stan files.
Its current features include:

- syntax highlighting
- indentation
- `compilation-mode` support
- `imenu-mode` support
- `flymake-mode` support
- `autocomplete-mode` support (if installed)

Two other packages contain additional functionality but are dependent on additional Emacs packages.

- `stan-snippets`: Adds [yasnippet](https://github.com/capitaomorte/yasnippet) snippets


## Installing

### Via package.el

The is the recommended way to install `stan-mode` is using the built-in package manager of Emacs 24, `package.el`. This allows for easy updating of `stan-mode` from within emacs.
If you are using Emacs 23, you will need to install `package.el`.

The stan packages are available from [MELPA](http://melpa.milkbox.net).
If you're not already using MELPA, follow its installation [instructions](http://melpa.milkbox.net/#/getting-started).

You can install the modes with the following commands:

<kbd>M-x package-install [RET] stan-mode [RET]</kbd>

<kbd>M-x package-install [RET] stan-snippets [RET]</kbd>

Or add the following to your Emacs initialization file (`.emacs` or `init.el`):

```el
(package-refresh-contents)
(mapc
 (lambda (p)
   (unless (package-installed-p p)
     (package-install p)))
 '(stan-mode stan-snippets))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents [RET]</kbd>

For more information on `package.el`, see the
[EmacsWiki](http://emacswiki.org/emacs/ELPA)

### Manually

Download the files to a local directory, and add lines to your `.emacs` file to add that directory to the `load-path`, and `require` the library.

For example, use the `git clone` command, which will create a directory `stan-mode`,
```console
$ cd ~/.emacs.d/plugins
$ git clone git://github.com/stan-dev/stan-mode.git
```

Include the following lines in your `.emacs` file,
```el
(add-to-list 'load-path "~/.emacs.d/plugins/stan-mode/")
```

For Aquamacs on Mac OS X, those lines alternatively could also be placed in the following preferences file `~/Library/Preferences/Aquamacs Emacs/Preferences.el`.

## Usage

Add the following line to your Emacs initialization file,
```el
(require 'stan-mode)
```
If you have [autocomplete](http://cx4a.org/software/auto-complete/) installed and would like to activate `stan-mode`'s support for it, add the following line
```el
(setq stan-use-auto-complete t)
```

``stan-mode`` supports flymake for on the fly syntax checking.
This requires a `stanc` executable, as built with ``CmdStan``.
If `stanc` is not in your `PATH` you need to either add it or set the variable `stan-stanc-path` to its location,
```el
(setq stan-stanc-path "/path/to/stanc")
```

Support for yasnippet snippets is contained in a separate package ``stan-snippets``.
This contains snippets to complete **all** functions with their arguments, as well as blocks, and a few other elements of the language.
You can activate snippet support by adding the following line to your Emacs initialization file,
```el
(require 'stan-snippets)
```


## Developers

This may be of use to developers which would like to support the Stan modeling language in other editors or applications.
The file `stan_lang.json` contains Stan keywords as well as the signatures and documentation of all the functions.

## License

`stan-mode` is free software under the [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html).

<!--  LocalWords:  stan imenu yasnippet flymake MELPA kbd RET init '
 -->
<!--  LocalWords:  mapc EmacsWiki cd 'load 'stan 'flymake Aquamacs 
 -->
<!--  LocalWords:  GPL stanc ' 'load 'stan autocomplete setq 'flymake
 -->
<!--  LocalWords:  lang json el emacs
 -->
