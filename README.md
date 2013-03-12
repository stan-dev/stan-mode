# stan-mode

Emacs major for [Stan](https://code.google.com/p/stan/).

Current features

- syntax highlighting
- indentation
- compilation-mode support
- imenu-mode support
- [yasnippet](https://github.com/capitaomorte/yasnippet) snippets
- [flymake-mode](http://flymake.sourceforge.net/) support

## Installing Emacs Mode

There are two mechanisms that can be used to install `stan-mode`.

## Package.el

The is the recommended way to install `stan-mode` is using the
built-in package manager of Emacs 24, `package.el`. This allows for
easy updating of `stan-mode` from within emacs. If you are using Emacs
23, you will need to install `package.el`.


Add the following to your `.emacs` file, which will ensure that
`package` is aware of the
[MELPA](https://github.com/milkypostman/melpa) repository, and that
`stan-mode` is installed.

```lisp
(require 'package)
(add-to-list 'package-archives
 '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless (package-installed-p 'scala-mode2)
(package-refresh-contents) (package-install 'scala-mode2))
```

For more information on `package.el`, see the
[EmacsWiki](http://emacswiki.org/emacs/ELPA).

### Manual


Download the files to a local directory, and add lines to your `.emacs`
file to add that directory to the `load-path`, and `require` the
library.

For example, use the `git clone` command, which will create a
directory `stan-mode`,
```console
$ cd ~/.emacs.d/plugins
$ git clone git://github.com/stan-dev/stan-mode.git
```

Include the following lines in your `.emacs` file,
```cl
(add-to-list 'load-path "~/.emacs.d/plugins/stan-mode/")
(require 'stan-mode)
;; Uncomment to activate yasnippet support (requires yasnippet)
;; (require 'stan-snippets)
;; Uncomment to activate flymake support (requires flymake)
;; (require 'stan-flymake)
```

For Aquamacs on Mac OS X, those lines alternatively could also be
placed in the following preferences file `~/Library/Preferences/Aquamacs Emacs/Preferences.el`.

## License

`stan-mode` is free software under the [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html).
