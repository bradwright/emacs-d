;;; Global keyboard combinations

;; Unset GNUs since it clashes with C-x C-m and I don't use it
(global-unset-key (kbd "C-x m"))
;; unset M-x due to smex
(global-unset-key (kbd "M-x"))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; Copy readline's kill word
(global-set-key (kbd "C-w") 'backward-kill-word)
;; Since we've unset C-w, map it to something else
(global-set-key (kbd "C-x C-k") 'kill-region)
;; ... and the clumsy version
(global-set-key (kbd "C-c C-k") 'kill-region)

(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; prompt before quitting
(global-set-key (kbd "C-x C-c") 'bw-kill-emacs)
(global-set-key (kbd "C-c x") 'server-edit)

(global-set-key (kbd "C-c m") 'manual-entry)

(global-set-key (kbd "C-c o") 'occur)
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

;; my custom search/find-file functions
(global-set-key (kbd "C-c b f") 'bw-find-file-git-ls-files-completing)
(global-set-key (kbd "C-c b s") 'bw-git-grep)

;; terminal-related bindings
(global-set-key (kbd "C-c t") 'bw-open-term)
(global-set-key (kbd "C-c C-t t") 'ansi-term)
(global-set-key (kbd "C-c C-t e") 'eshell)

;; mnemonic keybindings for window splitting (copies my tmux bindings)
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x -") 'split-window-below)

(global-set-key (kbd "C-c C-b") 'org-iswitchb)

(global-set-key (kbd "C-c s") 'bw-switch-to-scratch)

(global-set-key (kbd "C-h C-f") 'find-function)

(global-set-key (kbd "C-c f") 'bw-git-grep)
(global-set-key (kbd "M-p") 'bw-find-file-git-ls-files-completing)

(provide 'init-keybindings)
