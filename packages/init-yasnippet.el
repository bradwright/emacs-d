(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (progn
    (yas-global-mode 1)

    ;; we don't want yasnippet running in terminals
    (add-hook 'term-mode-hook (lambda()
                                (yas-minor-mode -1)))

    (setq yas/prompt-functions '(yas/ido-prompt yas/completing-prompt))))

(provide 'init-yasnippet)
