(use-package evil
  :config
  (progn
    ;; use ido to open files
    (define-key evil-ex-map "e " 'ido-find-file)
    (define-key evil-ex-map "b " 'ido-switch-buffer)

    ;; leader
    (let ((evil-leader-map (make-sparse-keymap)))
      ;; magit status on <leader>g
      (define-key evil-leader-map "g" 'magit-status)
      ;; git-ls-files on <leader>l
      (define-key evil-leader-map "l" 'bw-find-file-git-ls-files-completing)
      ;; git-grep-files on <leader>s
      (define-key evil-leader-map "s" 'bw-git-grep)
      ;; comma is leader
      (define-key evil-normal-state-map "," evil-leader-map))

    (setq evil-default-state 'emacs)
    (evil-set-initial-state 'git-commit-mode 'emacs)
    (evil-set-initial-state 'magit-log-edit-mode 'emacs)))

(provide 'init-evil)
