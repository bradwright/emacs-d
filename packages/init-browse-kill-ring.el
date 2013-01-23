(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring)
  :config (setq browse-kill-ring-replace-yank t) ; act like yank-pop
  )

(provide 'init-browse-kill-ring)
