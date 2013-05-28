(use-package flx
  :init
  (progn
    (setq gc-cons-threshold 20000000)
    (flx-ido-mode 1)))

(provide 'init-flx)
