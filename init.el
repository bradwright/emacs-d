;; My Emacs.d
;;
;; Intended to be robust and usable across platforms

;; No tabs
(setq-default indent-tabs-mode nil)

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

;; Load other files, C/O Emacs Starter Kit
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)

(load "grail")