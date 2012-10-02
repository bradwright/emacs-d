;;; -*- lexical-binding: t -*-

;; Keybindings, lots stolen from Emacs Starter Kit

;; also some wisdom taken from:
;; https://sites.google.com/site/steveyegge2/effective-emacs

;; You know, like Readline.
(global-set-key (kbd "C-w") 'backward-kill-word)
;; Since we've unset C-w, map it to something else
(global-set-key (kbd "C-x C-k") 'kill-region)
;; ... and the clumsy version
(global-set-key (kbd "C-c C-k") 'kill-region)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; External applications
(global-set-key (kbd "C-c t") 'bw-start-term)

;; map M-x to C-x C-m and C-c C-m
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
;; Unset GNUs since it clashes with above and I don't use it
(global-unset-key (kbd "C-x m"))
;; unset M-x due to above
(global-unset-key (kbd "M-x"))

;; Navigation Flymake errors
(global-set-key (kbd "C-c n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c p") 'flymake-goto-prev-error)

;; remap server/client exit commands
(global-set-key (kbd "C-x C-c") 'bw-kill-emacs)
(global-set-key (kbd "C-c x") 'server-edit)

;; Org-mode
(global-set-key (kbd "C-M-r") 'org-capture)

;; use ido search over imenu
(global-set-key (kbd "C-c i") 'idomenu)

;; we want an eshell easily
(global-set-key (kbd "C-c e") 'eshell)
;; always pop a new eshell
(global-set-key (kbd "C-c E") (lambda () (interactive) (eshell t)))

;; open URL
(when (or *is-a-mac* (display-graphic-p))
  (global-set-key (kbd "C-c b") 'browse-url))

;; we pretty much never ever want to background emacs
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; read MAN entries
(global-set-key (kbd "C-c m") 'manual-entry)

;; full screen
(global-set-key (kbd "C-c C-o") 'ns-toggle-fullscreen)
