(use-package evil
  :init
  (progn
    ;; if we don't have this evil overwrites the cursor color
    (setq evil-default-cursor t)

    ;; leader shortcuts
    (use-package evil-leader
      :init (global-evil-leader-mode)
      :config
      (progn
        ;; keyboard shortcuts
        (evil-leader/set-key
         "e" 'ido-find-file
         "b" 'ido-switch-buffer
         "g" 'magit-status
         "l" 'bw-find-file-git-ls-files-completing
         "s" 'ag-project
         "x" 'smex
         "a" 'ag)))

    ;; boot evil by default
    (evil-mode 1))
  :config
  (progn
    ;; use ido to open files
    (define-key evil-ex-map "e " 'ido-find-file)
    (define-key evil-ex-map "b " 'ido-switch-buffer)

    ;; jj escapes to normal mode
    (define-key evil-insert-state-map (kbd "j") 'bw-evil-escape-if-next-char-is-j)
    ;; h/l wrap around to next lines
    (setq evil-cross-lines t)
    ;; start evil-mode in emacs mode
    (setq evil-default-state 'emacs)

    ;; esc should always quit: http://stackoverflow.com/a/10166400/61435
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

    ;; ansi-term and term-mode have to be in Emacs
    (evil-set-initial-state 'term-mode 'emacs)
    (evil-set-initial-state 'eshell-mode 'emacs)))

(provide 'init-evil)
