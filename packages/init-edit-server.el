(when (require 'edit-server nil t)
  (edit-server-start))
(setq edit-server-url-major-mode-alist
      '(("github\\.com" . markdown-mode)))

(provide 'init-edit-server)
