;; map M-x to C-x C-m and C-c C-m, because M-x is in an awkward spot
(use-package smex
  :bind (("C-x C-m" . smex)
         ("C-c C-m" . smex))
  :config (setq smex-save-file (expand-file-name ".smex-items" tmp-local-dir)))

(provide 'init-smex)
