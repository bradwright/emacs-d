(use-package ack-and-a-half
  :init
  (progn
    (setq ack-and-a-half-regexp-search nil)
    (put 'ack-and-a-half-arguments 'safe-local-variable 'listp))
  :bind ("C-c C-a" . ack-and-a-half)
  :config
  (progn
    (add-to-list 'ack-and-a-half-arguments "--nopager")
    (setq ack-and-a-half-prompt-for-directory t)
    (add-hook 'eproject-first-buffer-hook (lambda ()
                                            (when (eproject-root)
      (set (make-local-variable 'ack-and-a-half-root-directory-functions) 'ack-and-a-half-root-directory-functions)
      (add-to-list 'ack-and-a-half-root-directory-functions 'eproject-root))))))

(provide 'init-ack-and-a-half)
