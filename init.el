;;; -*- lexical-binding: t -*-

;; cl seems to be used by everything
(require 'cl)

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

;; use-package - used in other places
(require 'init-use-package)

;; backups and autosaves
(require 'init-backups)

;; Editing and interface changes
(require 'init-editing)
(require 'init-interface)
(use-package init-window-gui
             :if (display-graphic-p))
(use-package init-osx
             :if *is-a-mac*)
(use-package init-linux
             :if *is-linux*)

;; Mode configuration

;; built-in modes
(require 'init-ido)
(require 'init-tramp)
(require 'init-ediff)
(require 'init-uniquify)
(require 'init-ansi-term)
(require 'init-eshell)
(require 'init-hippie-expand)
(require 'init-ruby)
(require 'init-org)
(require 'init-recentf)

;; vendor-ised modes
(require 'init-magit)
(require 'init-js2)

;;; Global keyboard combinations

;; map M-x to C-x C-m and C-c C-m, because M-x is in an awkward spot
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
;; Unset GNUs since it clashes with above and I don't use it
(global-unset-key (kbd "C-x m"))
;; unset M-x due to above
(global-unset-key (kbd "M-x"))

;; Use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; Copy readline's kill word
(global-set-key (kbd "C-w") 'backward-kill-word)
;; Since we've unset C-w, map it to something else
(global-set-key (kbd "C-x C-k") 'kill-region)
;; ... and the clumsy version
(global-set-key (kbd "C-c C-k") 'kill-region)

(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; start a server, unless one is already running
(require 'server)
(unless (server-running-p)
  (server-start))
