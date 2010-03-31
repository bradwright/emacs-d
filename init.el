;; My Emacs.d
;;
;; Intended to be robust and usable across platforms

;; Load external files
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/vendor"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;; load on startup
(require 'cl)
(require 'saveplace)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'magit)

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
(setq-default tab-width 4)

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
   backup-directory-alist `((".*" . ,(concat dotfiles-dir "backups")))
   auto-save-file-name-transforms `((".*" ,(concat dotfiles-dir "autosaves") t))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)   ; Use versioned backups

;; show trailing whitespace
(setq show-trailing-whitespace t)
(whitespace-mode t)
(setq whitespace-style '(trailing lines space-before-tab
                         indentation space-after-tab)
      whitespace-line-column 100)

;; show path rather than <2>
(setq uniquify-buffer-name-style 'forward)

;; show keystrokes immediately
(setq echo-keystrokes 0.1)

;; Fuck auto fill
(setq auto-fill-mode nil)

;; nXhtml
;;(load "nxhtml/autostart.el")

(autoload 'yaml-mode "yaml" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; JS2 mode, not espresso
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-highlight-level 3
      js2-basic-offset 4)

;; Jinja mode is a bit crap, really
;;(load "jinja")
;;(add-to-list 'auto-mode-alist '("\\.jinja$" . jinja-mode))

;; Show colours in magit
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

;; Restructured text
(require 'rst)
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))

;; My functions
(load "elisp")

;; My keyboard shortcuts
(load "keys")

;; Platform specific stuff
(when window-system
    (load "gui"))

(when (eq system-type 'darwin)
    (load "darwin"))