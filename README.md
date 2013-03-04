stan-mode
=========

Emacs mode for Stan.

Installing Emacs Mode
-------------------------------

Typically, emacs is set up to read a <code>.emacs</code> file from the
user's home directory.  Add the following two lines to this file
replacing <code>&lt;stan-home&gt;</code>
with the path to where Stan was unpacked.


(add-to-list 'load-path "/Users/carp/stan/src/StanMode/")
(require 'stan-mode)

For Aquamacs on Mac OS X, these two lines can be placed in the
<code>.emacs</code> file or in the following preferences file.

~/Library/Preferences/Aquamacs Emacs/Preferences.el
