stan-mode
=========

Emacs major for [Stan](https://code.google.com/p/stan/).

Current features

- syntax highlighting
- indentation
- compilation-mode support
- imenu-mode support
- [yasnippet](https://github.com/capitaomorte/yasnippet) snippets
- [flymake-mode](http://flymake.sourceforge.net/) support

Installing Emacs Mode
-------------------------------

Download the files to a local directory, and add lines to you `.emacs`
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

License
---------------

`stan-mode` is free software under the [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html).
