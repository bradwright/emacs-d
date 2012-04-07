==========================
  My Emacs.d configuration
==========================

As my editor of choice I run Emacs. One of the benefits of Emacs is that
it'll install basically anywhere.

This configuration is designed to run in shells as well as on
desktops. There are some platform specific optimisations as well.

Installing this Emacs
=====================

Just check it out straight to the right directory, and use Make to
install it:

::

    cd ~/Projects/emacs-d && make


Platform specific configuration
===============================

OS X desktop (Emacs.app)
------------------------

* Left `option` key is remapped to `M-`
* `M-3` prints a literal `#` (UK Mac keyboards being weird)
* We use the `Solarized Dark`_ theme
* Font is `Inconsolata`_

.. _`Solarized Dark`: https://github.com/sellout/emacs-color-theme-solarized
.. _`Inconsolata`: http://www.levien.com/type/myfonts/inconsolata.html

OS X CLI in iTerm2
------------------

* When the `Solarized Dark iTerm2 theme`_ is installed, we use the
  `solarized-dark` color theme

  * Re-map left `option` key to `+Esc`

* Cut/paste are made to write/read from the clipboard (via
  `pbcopy` and `pbpaste`)
* Mouse highlighting works via xTerm capabilities

.. _`Solarized Dark iTerm2 theme`: https://github.com/altercation/solarized/blob/master/iterm2-colors-solarized/Solarized%20Dark.itermcolors

Included libraries
==================

The following libraries are included in non-attributable ways, i.e not
via package install or via a Git submodule:

* `Tomorrow`_, an Emacs theme;

.. _`Tomorrow`: https://github.com/ChrisKempson/Tomorrow-Theme
