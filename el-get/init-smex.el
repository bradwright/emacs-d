;; map M-x to C-x C-m and C-c C-m, because M-x is in an awkward spot
(use-package smex
  :bind (("C-x C-m" . smex)
         ("C-c C-m" . smex)))

(provide 'init-smex)
