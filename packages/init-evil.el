(use-package evil
  :config
  (progn
    (setq evil-default-state 'emacs)
    (evil-set-initial-state 'git-commit-mode 'emacs)
    (evil-set-initial-state 'magit-log-edit-mode 'emacs)))

(provide 'init-evil)
