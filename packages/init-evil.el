(use-package evil
  :config
  (progn
    ;; use ido to open files
    (define-key evil-ex-map "e " 'ido-find-file)
    (define-key evil-ex-map "b " 'ido-switch-buffer)

    (setq evil-default-state 'emacs)
    (evil-set-initial-state 'git-commit-mode 'emacs)
    (evil-set-initial-state 'magit-log-edit-mode 'emacs)))

(provide 'init-evil)
