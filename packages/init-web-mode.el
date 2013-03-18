(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :config
  (progn
    (setq web-mode-engines-alist
          '(("\\.jinja\\'"  . "django")))))

(provide 'init-web-mode)
