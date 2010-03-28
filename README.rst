==========================
  My Emacs.d configuration
==========================

As my editor of choice I run Emacs. One of the benefits of Emacs is that it'll install basically anywhere.

This configuration is designed to run in shells as well as on desktops. There are some platform specific optimisations as well. Most of the configuration is down to Grail_: I can't claim any of the coding that makes this configuration fork by platform.

.. _Grail: http://www.emacswiki.org/emacs/Grail

Installing this Emacs
=====================

Because we're using Grail, you need to perform some trickery to get this to work. Assuming you've checked out this repository in ``~/.emacs-d``:

::
    
    ln -s $HOME/.emacs-d/grail.el ~/.emacs
    export USER_ELISP=$HOME/.emacs-d/
    
Then you need to compile JS2-mode:

::

    cd ~/.emacs-d/dist/elisp/
    emacs -q --batch --eval '(byte-compile-file "js2.el")'

Running Emacs in quiet mode is required because of `this issue`_.

.. _this issue: http://code.google.com/p/js2-mode/issues/detail?id=68
    