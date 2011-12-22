;; My Emacs.d
;;
;; Intended to be robust and usable across platforms

;; Load external files
(setq
 dotfiles-dir
 (file-name-directory
  (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

;; vendor files
(setq
 vendor-dotfiles-dir
 (concat dotfiles-dir "/vendor"))
(add-to-list 'load-path vendor-dotfiles-dir)

;; tmp directory for storing stupid crap
(setq
 tmp-local-dir
 (concat dotfiles-dir "/.tmp"))

;; load on startup
(require 'cl)
(require 'saveplace)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'whitespace)

;; kill all start up stuff
(setq inhibit-startup-screen t)
(setq initial-buffer-choice t)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; always highlight syntax
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; hook to turn off hl-line-mode
(defun local-hl-line-mode-off ()
  (interactive)
  (make-local-variable 'global-hl-line-mode)
  (setq global-hl-line-mode nil))

;; Save a list of recent files visited.
;; disable auto-clean before we start recentf so Tramp doesn't block emacs
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

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
 backup-by-copying t  ; Don't clobber symlinks
 backup-directory-alist `((".*" . ,(concat tmp-local-dir "/backups")))
 auto-save-file-name-transforms `((".*" ,(concat tmp-local-dir "/autosaves") t))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)   ; Use versioned backups


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

;; face for Tabs
(set-face-attribute 'whitespace-tab nil
                    :background "red1"
                    :foreground "yellow"
                    :weight 'bold)

;; enable whitespace mode
(global-whitespace-mode t)
(whitespace-mode t)

;; show path rather than <2>
(setq uniquify-buffer-name-style 'forward)

;; show keystrokes immediately
(setq echo-keystrokes 0.1)

;; Fuck auto fill
(setq auto-fill-mode nil)

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; going past the bottom of the bugger adds a new line
(setq next-line-add-newlines t)

;; My functions
(load "elisp")

;; My keyboard shortcuts
(load "keys")

;; start a server
(load "server")
(unless (server-running-p) (server-start))

;; we load modes last, because things above might have changed how we load them
;; Major/minor modes
(load "modes")

;; Platform specific stuff
(when window-system
  (load "gui"))
(when (eq system-type 'darwin)
  (load "darwin"))
(when (and (eq system-type 'darwin) (not window-system))
  (load "darwin-cli"))
(when (and (eq system-type 'darwin) (window-system))
  (load "darwin-gui"))

;; Load custom file last
(if (file-exists-p (concat dotfiles-dir "custom.el"))
    (progn
      (setq custom-file (concat dotfiles-dir "custom.el"))
      (load-file custom-file)))
