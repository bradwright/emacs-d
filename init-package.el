(use-package package
  :config
  (progn
    (setq
     package-archives
     '(("gnu" . "http://elpa.gnu.org/packages/")
       ("marmalade" . "http://marmalade-repo.org/packages/")
       ("melpa" . "http://melpa.milkbox.net/packages/")))))

(provide 'init-package)
