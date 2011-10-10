;; Keybindings, lots stolen from Emacs Starter Kit
;; also some wisdom taken from https://sites.google.com/site/steveyegge2/effective-emacs

;; You know, like Readline.
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; External applications
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x t") 'ansi-term)

;; Window movement
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two
(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-tab] (lambda () (interactive) (other-window -1)))

;; map M-x to C-x C-m and C-c C-m
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

;; force myself to stop using M-x
(global-set-key
 (kbd "M-x")
 (lambda ()
   (interactive)
   (message "Use C-x C-m!")))

;; anti-n00b mode
(defun toggle-noob-mode ()
  "Toggles turning off of arrow keys"
  (interactive)
  (if (not (boundp 'noob-keyboard-mode))
      (setq noob-keyboard-mode nil))
  (if noob-keyboard-mode
      (setq noob-keyboard-mode nil)
    (setq noob-keyboard-mode t))
  (if noob-keyboard-mode
      ;; real Emacs hackers don't use the arrow keys
      (progn
        (global-set-key
         (kbd "<up>")
         (lambda ()
           (interactive)
           (message "Arrow key navigation is disabled. Use C-p instead.")))
        (global-set-key
         (kbd "<down>")
         (lambda ()
           (interactive)
           (message "Arrow key navigation is disabled. Use C-n instead.")))
        (global-set-key
         (kbd "<left>")
         (lambda ()
           (interactive)
           (message "Arrow key navigation is disabled. Use C-b instead.")))
        (global-set-key
         (kbd "<right>")
         (lambda ()
           (interactive)
           (message "Arrow key navigation is disabled. Use C-f instead.")))
        (message "Arrow keys now disabled"))
    (progn
      (global-set-key (kbd "<up>") 'previous-line)
      (global-set-key (kbd "<down>") 'next-line)
      (global-set-key (kbd "<left>") 'backward-char)
      (global-set-key (kbd "<right>") 'forward-char)
      (message "Arrow keys now work"))))

(toggle-noob-mode)
