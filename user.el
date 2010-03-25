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

;; JS2 mode, not espresso
(load-file "~/.emacs-d/dist/elisp/js2.el")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Jinja mode is a bit crap, really
(load-file "~/.emacs-d/dist/elisp/jinja.el")
(add-to-list 'auto-mode-alist '("\\.jinja$" . jinja-mode))