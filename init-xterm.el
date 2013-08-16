;; xterm configuration

;; xterm-frobs is a vendorised library
(use-package xterm-frobs
  :init
  (progn
    (defun bw-xterm-title ()
      (xterm-set-window-title (concat "emacs@" (system-name)))
      (xterm-set-icon-title (concat "emacs: " (buffer-name))))
    (add-hook 'window-configuration-change-hook 'bw-xterm-title)))

(provide 'init-xterm)
