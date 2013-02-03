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
      ;; smex is <leader>x
      (define-key evil-leader-map "x" 'smex)
      ;; ido-switch-buffer is <leader>b
      (define-key evil-leader-map "b" 'ido-switch-buffer)
      ;; ack-and-a-half
      (define-key evil-leader-map "a" 'ack-and-a-half)
      ;; comma is leader
      (define-key evil-normal-state-map "," evil-leader-map))

    (setq evil-default-state 'normal)

    ;; these modes should be launched in insert
    (evil-set-initial-state 'git-commit-mode 'insert)
    (evil-set-initial-state 'magit-log-edit-mode 'insert)
    (evil-set-initial-state 'help-mode 'emacs)))

(provide 'init-evil)
