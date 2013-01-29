(use-package evil
  :config
  (progn
    (evil-set-initial-state 'git-commit-mode 'insert)
    (evil-set-initial-state 'magit-log-edit-mode 'insert)))

(provide 'init-evil)
