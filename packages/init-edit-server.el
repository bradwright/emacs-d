(use-package edit-server
  :init (progn
          (edit-server-start)
          (setq edit-server-url-major-mode-alist
                '(("github\\.com" . markdown-mode)))))

(provide 'init-edit-server)
