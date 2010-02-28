==========================
  My Emacs.d configuration
==========================

As my editor of choice I run Emacs. One of the benefits of Emacs is that it'll install basically anywhere.

This configuration is designed to run in shells as well as on desktops. There are some platform specific optimisations as well.

Installing this Emacs
=====================

Because we're using Grail, you need to perform some trickery to get this to work:

::
    
    ln -s $HOME/Projects/emacs-d/grail.el ~/.emacs
    export USER_ELISP=/Users/bradleyw/Projects/emacs-d/