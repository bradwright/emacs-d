;;; -*- lexical-binding: t -*-

;; My Emacs.d
;;
;; Intended to be robust and usable across platforms

;; borrowed from:
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

;; My functions
(load "elisp")

;; external libraries I might have collected via submodules etc.
(setq vendor-dotfiles-dir (bw-join-dirs dotfiles-dir "vendor"))
(bw-add-to-load-path vendor-dotfiles-dir)

;; automatically add everything under vendor to load-path
(dolist (f (directory-files vendor-dotfiles-dir))
  (let ((name (concat vendor-dotfiles-dir "/" f)))
    (when (and (file-directory-p name)
               (not (equal f ".."))
               (not (equal f ".")))
      (bw-add-to-load-path name))))

;; use-package
(require 'use-package)
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))

;; tmp directory for storing stupid crap
(make-directory (setq tmp-local-dir (bw-join-dirs dotfiles-dir ".tmp/")) t)

(use-package recentf
  :init (recentf-mode 1)
  :config
  (progn
    ;; Save a list of recent files visited.
    ;; disable auto-clean before we start recentf so Tramp doesn't block emacs
    (setq recentf-auto-cleanup 'never
          recentf-exclude '("[/\\]\\.elpa/" "[/\\]\\.ido\\.last\\'" "[/\\]\\.git/"))))

;; I got sick of typing "yes"
(defalias 'yes-or-no-p 'y-or-n-p)

;; No tabs
(setq-default
 indent-tabs-mode nil
 tab-width 4)

;; UTF-8 please!
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; don't clobber copy/paste things
(setq save-interprogram-paste-before-kill t)

;; Stop autosaves and backups from littering the filesystem
;; Keep backups in same dir
(setq
 tmp-backups-dir (bw-join-dirs tmp-local-dir "backups")
 tmp-autosaves-dir (bw-join-dirs tmp-local-dir "autosaves"))

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

;; http://emacs-fu.blogspot.hk/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position
            2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
            (line-beginning-position 2)))))

;; always rescan
(set-default 'imenu-auto-rescan t)

;; My keyboard shortcuts
(load "keys")

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

;; load local files
;; local overrides
(setq local-dotfiles-dir (bw-join-dirs dotfiles-dir "local"))

(setq
 bw-user-config (concat local-dotfiles-dir user-login-name ".el")
 bw-system-config (concat local-dotfiles-dir system-name ".el"))

(defun load-local-configs ()
  (when (file-exists-p bw-user-config)
    (load bw-user-config))
  (when (file-exists-p bw-system-config)
    (load bw-system-config)))

(add-hook 'after-init-hook 'load-local-configs)

;; Load custom file last
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)
