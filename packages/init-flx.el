(use-package flx
  :init
  (progn
    (setq gc-cons-threshold 20000000)
    (use-package flx-ido
      :init
      (flx-ido-mode 1))))

(provide 'init-flx)
