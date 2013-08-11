;; this is basically like Vim's relative numbers

(use-package linum-relative
  :init
  (progn
    ;; turn on linum-mode, and make it relative
    (add-hook 'evil-normal-state-entry-hook (lambda ()
                                              (setq linum-format 'linum-relative)

                                              (linum-mode 1)))

    ;; turn off linum-mode, and make it normal again
    (add-hook 'evil-normal-state-exit-hook (lambda ()
                                             (setq linum-format 'dynamic)
                                             (linum-mode -1)))

    ;; copy linum face so it doesn't look weird
    (custom-set-faces
     '(linum-relative-current-face
       ((t (:inherit linum :weight bold :reverse t))))))

  :config (setq linum-relative-current-symbol ">>"))

(provide 'init-linum-relative)
