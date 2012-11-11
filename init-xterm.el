;; xterm configuration

;; xterm-frobs is a vendorised library
(use-package xterm-frobs
  :if (not (display-graphic-p))
  :init
  (progn
    (defun bw-xterm-title ()
      (xterm-set-window-title (concat "emacs@" (system-name)))
      (xterm-set-icon-title (buffer-name)))
    (add-hook 'window-configuration-change-hook 'bw-xterm-title)))

(provide 'init-xterm)
