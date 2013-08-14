(use-package ido-vertical-mode
  :init (progn
          (ido-vertical-mode 1)
          ;; only show 5 so the list isn't too high
          (setq ido-max-prospects 5)))

(provide 'init-ido-vertical)
