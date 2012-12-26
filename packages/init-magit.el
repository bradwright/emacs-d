;; Magit is an Emacs interface to Git.
;; (It's awesome)
;; https://github.com/magit/magit

;; This is vendor-ised because I want the info files to be compiled
;; along with the Emacs Lisp files, so that I can read the manual in
;; Emacs.

(use-package magit
  :bind ("C-c g" . magit-status)
  :init
  (progn
    ;; make magit status go full-screen but remember previous window
    ;; settings

    ;; from: http://whattheemacsd.com/setup-magit.el-01.html
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (delete 'Git vc-handled-backends))
  :config
  (progn
    ;; magit extensions
    ;; restore previously hidden windows
    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))

    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)


    (use-package magit-blame
      :bind ("C-c C-g b" . magit-blame-mode))

    ;; major mode for editing `git rebase -i` files
    (use-package rebase-mode)

    ;; magit settings
    (setq
     ;; use ido to look for branches
     magit-completing-read-function 'magit-ido-completing-read
     ;; don't put "origin-" in front of new branch names by default
     magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
     ;; open magit status in same window as current buffer
     magit-status-buffer-switch-function 'switch-to-buffer
     ;; highlight word/letter changes in hunk diffs
     magit-diff-refine-hunk t
     ;; ask me if I want to include a revision when rewriting
     magit-rewrite-inclusive 'ask
     ;; don't always save buffers
     magit-save-some-buffers nil
     ;; pop the process buffer if we're taking a while to complete
     magit-process-popup-time 10)))

(provide 'init-magit)
