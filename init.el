;; My Emacs.d
;;
;; Intended to be robust and usable across platforms

;; Load external files
(setq
 dotfiles-dir
 (file-name-directory
  (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

;; load on startup
(require 'cl)
(require 'saveplace)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'magit)
(require 'whitespace)
(require 'tramp)

;; auto decrypt PGP encrypted files
(require 'epa)
(epa-file-enable)

;; kill all start up stuff
(setq inhibit-startup-screen t)
(setq initial-buffer-choice t)

;; You really don't need this; trust me.
(menu-bar-mode -1)
(tool-bar-mode -1)

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

;; IDO mode is awesome
(ido-mode t)
(setq
 ido-enable-prefix nil
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


;; Whitespace mode from:
;; http://ruslanspivak.com/2010/09/27/keep-track-of-whitespaces-and-column-80-overflow/

;; nuke trailing whitespaces when writing to a file
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; display only tails of lines longer than 80 columns, tabs and
;; trailing whitespaces
(setq whitespace-line-column 80
      whitespace-style '(tabs trailing lines-tail))

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

;; nXhtml
;;(load "nxhtml/autostart.el")

(autoload 'yaml-mode "yaml" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; JS2 mode, not espresso
(autoload 'js2-mode "js2" nil t)
;;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq
 js2-highlight-level 3
 js2-basic-offset 4)

;; Jinja mode is a bit crap, really
;;(load "jinja")
;;(add-to-list 'auto-mode-alist '("\\.jinja$" . jinja-mode))
;; Replace Jinja mode with HTML mode, since Jinja mode sucks
(add-to-list 'auto-mode-alist '("\\.jinja$" . html-mode))

(defun my-html-mode-hook ()
  (setq tab-width 4)
  (auto-fill-mode 0)
  (define-key html-mode-map (kbd "<tab>") 'my-insert-tab)
  (define-key html-mode-map (kbd "C->") 'sgml-close-tag))

;; just insert tabs
(defun my-insert-tab (&optional arg)
  (interactive "P")
  (insert-tab arg))

(add-hook 'html-mode-hook 'my-html-mode-hook)

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

;; start a server
(server-start)

;; Tramp mode: allow me to SSH to hosts and edit as sudo like:
;;   C-x C-f /sudo:example.com:/etc/something-owned-by-root
;; from: http://www.gnu.org/software/tramp/#Multi_002dhops
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))

;; Platform specific stuff
;; this is C/O: http://stackoverflow.com/questions/2548673/how-do-i-get-emacs-to-evaluate-a-file-when-a-frame-is-raised
(add-hook 'after-make-frame-functions
  (lambda (frame)
    (set-variable 'color-theme-is-global nil)
    (select-frame frame)
    (when window-system
      (load "gui"))
    (when (eq system-type 'darwin)
      (load "darwin"))
    ))

;; run the file anyway just in case...
(if window-system
    (progn
      (run-hook-with-args 'after-make-frame-functions (car (frame-list)))))

;; Load custom file last
(setq custom-file (concat dotfiles-dir "custom.el"))
(load-file custom-file)