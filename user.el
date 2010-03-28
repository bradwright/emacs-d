;; My Emacs.d
;;
;; Intended to be robust and usable across platforms

;; load on startup
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; kill all start up stuff
(setq inhibit-startup-screen t)
(setq initial-buffer-choice t)

;; You really don't need this; trust me.
(menu-bar-mode -1)

;; Save a list of recent files visited.
(recentf-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Hippie expand: at times perhaps too hip
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)

;; I got sick of typing "yes"
(defalias 'yes-or-no-p 'y-or-n-p)

;; No tabs
(setq-default indent-tabs-mode nil)

;; IDO mode is awesome
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10)

;; UTF-8 please!
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Stop autosaves and backups from littering the filesystem
;; Keep backups in same dir
(setq
   backup-by-copying t  ; Don't clobber symlinks
   backup-directory-alist '(("." . "backups/")) ; Don't litter FS
   auto-save-file-name-transforms '((".*" "autosaves/" t))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)   ; Use versioned backups

;; show trailing whitespace
(setq show-trailing-whitespace t)
(whitespace-mode t)


;; Load external files
(add-to-list 'load-path "~/.emacs-d/dist/elisp")

;; JS2 mode, not espresso
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-highlight-level 3
      js2-basic-offset 4)

;; Jinja mode is a bit crap, really
(load "jinja")
(add-to-list 'auto-mode-alist '("\\.jinja$" . jinja-mode))