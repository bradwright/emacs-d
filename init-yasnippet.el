(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (progn
    (yas-global-mode 1)
    (setq yas/prompt-functions '(yas/ido-prompt yas/completing-prompt))))

(provide 'init-yasnippet)
