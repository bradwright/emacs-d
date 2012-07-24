;;; -*- lexical-binding: t -*-

;; My Emacs.d
;;
;; Intended to be robust and usable across platforms

;; borrowed from:
;; https://github.com/purcell/emacs.d/blob/master/init.el
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))
(setq *is-linux* (eq system-type 'gnu/linux))

;; from emacs-starter-kit
(progn
  ;; Turn off mouse interface early in startup to avoid momentary display
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

;; annoying, a lot of stuff uses this
(require 'cl)

;; `dotfiles-dir` is the current directory
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
;; FIXME: do I need this?
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

;; external libraries I might have collected via submodules etc.
(setq vendor-dotfiles-dir (file-name-as-directory (concat dotfiles-dir "vendor/")))
(add-to-list 'load-path vendor-dotfiles-dir)
;; automatically add everything under vendor to load-path
(dolist (f (directory-files vendor-dotfiles-dir))
  (let ((name (concat vendor-dotfiles-dir "/" f)))
    (when (and (file-directory-p name)
               (not (equal f ".."))
               (not (equal f ".")))
      (add-to-list 'load-path name))))

;; use-package
(require 'use-package)
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))

;; tmp directory for storing stupid crap
(setq tmp-local-dir (file-name-as-directory (concat dotfiles-dir ".tmp/")))
(make-directory tmp-local-dir t)

;; kill all start up stuff
(setq inhibit-startup-screen t)
(setq initial-buffer-choice t)

;; always highlight syntax
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(use-package recentf
  :init (recentf-mode 1)
  :config
  (progn
    ;; Save a list of recent files visited.
    ;; disable auto-clean before we start recentf so Tramp doesn't block emacs
    (setq recentf-auto-cleanup 'never
          recentf-exclude '("[/\\]\\.elpa/"))))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; I got sick of typing "yes"
(defalias 'yes-or-no-p 'y-or-n-p)

;; No tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; UTF-8 please!
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Stop autosaves and backups from littering the filesystem
;; Keep backups in same dir
(setq
 tmp-backups-dir (file-name-as-directory (concat tmp-local-dir "backups"))
 tmp-autosaves-dir (file-name-as-directory (concat tmp-local-dir "autosaves")))

(make-directory tmp-backups-dir t)
(make-directory tmp-autosaves-dir t)

(setq
 backup-by-copying t  ; Don't clobber symlinks
 backup-directory-alist `((".*" . ,tmp-backups-dir))
 auto-save-file-name-transforms `((".*" ,tmp-autosaves-dir t))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)   ; Use versioned backups

;; nuke trailing whitespaces when writing to a file
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(use-package uniquify
  :config
  (progn
    ;; this shows foo/bar and baz/bar when two files are named bar
    (setq uniquify-buffer-name-style 'forward)
    ;; strip common suffixes
    (setq uniquify-strip-common-suffix t)))

;; show keystrokes immediately
(setq echo-keystrokes 0.1)

;; Fuck auto fill
(setq auto-fill-mode nil)

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; always rescan
(set-default 'imenu-auto-rescan t)

;; My functions
(load "elisp")

;; My keyboard shortcuts
(load "keys")

;; start a server
(use-package server
  :init
  (progn
    (unless (server-running-p)
      (server-start))))

;; packages
(load "packages")

;; we load modes last, because things above might have changed how we load them
;; Major/minor modes
(load "modes")

;; load generic GUI configuration
(load "gui")

;; OSX specific code
(when *is-a-mac*
  (load "darwin")
  (if (or *is-carbon-emacs*
          *is-cocoa-emacs*)
      (load "darwin-gui")
    (load "darwin-cli")))

(when *is-linux*
  (unless (display-graphic-p)
    (load "linux-cli")))

;; Load custom file last
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)
