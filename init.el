;;; -*- lexical-binding: t -*-

;;; general global variables for configuration

;; base load path
(defconst dotfiles-dir
  (file-name-directory
   (or (buffer-file-name) load-file-name))
  "Base path for customised Emacs configuration")

(add-to-list 'load-path dotfiles-dir)

;; What OS/window system am I using?

;; Adapted from:
;; https://github.com/purcell/emacs.d/blob/master/init.el
(defconst *is-a-mac*
  (eq system-type 'darwin)
  "Is this running on OS X?")

(defconst *is-carbon-emacs*
  (and *is-a-mac* (eq window-system 'mac))
  "Is this the Carbon port of Emacs?")

(defconst *is-cocoa-emacs*
  (and *is-a-mac* (eq window-system 'ns))
  "Is this the Cocoa version of Emacs?")

(defconst *is-linux*
  (eq system-type 'gnu/linux)
  "Is this running on Linux?")

;; Basic paths and variables other things need
(require 'init-utils)
(require 'init-paths)

;; backups and autosaves
(require 'init-backups)

;; Editing and interface changes
(require 'init-editing)
(require 'init-interface)

;; all external libraries, ELPA, el-get etc.
(require 'init-packaging)

;; use-package - used in other places
(require 'init-use-package)

;; Platform specific configuration
(use-package init-window-gui
  :if (display-graphic-p))
(use-package init-osx
  :if *is-a-mac*)
(use-package init-linux
  :if *is-linux*)
(use-package init-xterm
  :if (not (display-graphic-p)))

;; Mode configuration

;; built-in modes
(defconst core-modes
  '(init-abbrev
    init-ansi-color
    init-ansi-term
    init-conf
    init-ediff
    init-emacs-lisp
    init-eshell
    init-flymake
    init-hippie-expand
    init-ido
    init-isearch
    init-org
    init-recentf
    init-rst
    init-ruby
    init-saveplace
    init-tramp
    init-uniquify
    init-web-mode
    init-whitespace)
  "Configuration for core Emacs packages")

(bw-require-list core-modes)

;; vendor-ised modes
(defconst vendor-modes
  '(init-json)
  "Configuration for vendorised code")

(bw-require-list vendor-modes)

;; Packaged modes from ELPA etc.
(defconst elpa-modes
  '(init-ace-jump
    init-ack-and-a-half
    init-browse-kill-ring
    init-csv-mode
    init-eproject
    init-evil
    init-expand-region
    init-iedit
    init-ido-ubiquitous
    init-idomenu
    init-js2
    init-magit
    init-markdown
    init-multiple-cursors
    init-paredit
    init-puppet
    init-smex
    init-undo-tree
    init-yaml
    init-yasnippet)
  "Configuration for modes loaded via package.el")

(bw-require-list elpa-modes)

;; Custom theme support
(require 'init-themes)
(require 'init-solarized)

(require 'init-keybindings)

;; start a server, unless one is already running
(when (require 'server nil t)
  (unless (server-running-p)
    (server-start)))

;; local overrides
(require 'init-local)

;; Load custom file last
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

;; make sure we run the init hooks even if we didn't get a proper init
(when after-init-time
  (run-hooks 'after-init-hook))
