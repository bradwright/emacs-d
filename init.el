;;; -*- lexical-binding: t -*-

;; My Emacs.d
;;
;; Intended to be robust and usable across platforms

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

;; use-packacge
(add-to-list 'load-path (file-name-as-directory (concat vendor-dotfiles-dir "use-package")))
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

;; Save a list of recent files visited.
;; disable auto-clean before we start recentf so Tramp doesn't block emacs
(require 'recentf)
(setq recentf-auto-cleanup 'never
      recentf-exclude '("[/\\]\\.elpa/"))
(recentf-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; show lines after end of buffer
(setq indicate-empty-lines t)

;; Hippie expand: at times perhaps too hip
(delete
 'try-expand-line hippie-expand-try-functions-list)
(delete
 'try-expand-list hippie-expand-try-functions-list)

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

;; Whitespace mode
(require 'whitespace)
;; Whitespace mode from:
;; http://ruslanspivak.com/2010/09/27/keep-track-of-whitespaces-and-column-80-overflow/

;; nuke trailing whitespaces when writing to a file
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; display only tails of lines longer than 80 columns, tabs and
;; trailing whitespaces

;; style information is here: http://www.emacswiki.org/emacs/WhiteSpace
(setq whitespace-line-column 80
      whitespace-style '(face tabs trailing lines-tail))

;; face for long lines' tails
(set-face-attribute 'whitespace-line nil)

;; enable whitespace mode
(global-whitespace-mode t)
(whitespace-mode t)

(require 'uniquify)
;; this shows foo/bar and baz/bar when two files are named bar
(setq uniquify-buffer-name-style 'forward)
;; strip common suffixes
(setq uniquify-strip-common-suffix t)

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
(require 'server)
(unless (server-running-p)
  (server-start))

;; packages
(load "packages")

;; we load modes last, because things above might have changed how we load them
;; Major/minor modes
(load "modes")

;; load generic GUI configuration
(load "gui")

;; OSX specific code
(when (eq system-type 'darwin)
  (load "darwin")
  (if (display-graphic-p)
      (load "darwin-gui")
    (load "darwin-cli")))

(when (eq system-type 'gnu/linux)
  (unless (display-graphic-p)
    (load "linux-cli")))

;; Load custom file last
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)
