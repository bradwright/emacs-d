;; yaml-mode doesn't autoload
(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :config
  (progn
    (put 'yaml-indent-offset 'safe-local-variable 'integerp)))

(provide 'init-yaml)
