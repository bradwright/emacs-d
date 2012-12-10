(use-package ack-and-a-half
  :init
  (progn
    (setq
     ;; don't use regexes by default
     ack-and-a-half-regexp-search nil
     ;; my ackrc has a pager by default, so clobber it
     ack-and-a-half-arguments '("--nopager")
     ;; prompt for a directory instead of just accepting project root
     ack-and-a-half-prompt-for-directory t))
  :bind ("C-c C-a" . ack-and-a-half)
  :config
  (progn

    (add-hook 'eproject-first-buffer-hook (lambda ()
                                            (when (eproject-root)
                                              (set (make-local-variable 'ack-and-a-half-root-directory-functions) 'ack-and-a-half-root-directory-functions)
                                              (add-to-list 'ack-and-a-half-root-directory-functions 'eproject-root))))))

(provide 'init-ack-and-a-half)
