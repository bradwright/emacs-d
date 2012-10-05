;;; Magit

(use-package magit
  :bind ("C-c g" . magit-status)
  :init
  (progn
    (add-to-list 'Info-default-directory-list (bw-locate-library-dir "magit"))
    (delete 'Git vc-handled-backends))
  :config
  (progn
    ;; force wrap magit commit messages
    (add-hook 'magit-log-edit-mode-hook 'bw-turn-on-auto-fill)
    (add-hook 'git-commit-mode-hook 'bw-turn-on-auto-fill)

    (add-hook 'magit-log-edit-mode-hook 'bw-fill-column)
    (add-hook 'git-commit-mode-hook 'bw-fill-column)

    ;; magit extensions
    (use-package magit-blame
      :bind ("C-c C-g b" . magit-blame-mode))

    (use-package magithub
      :init)

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
     magit-save-some-buffers nil)))
