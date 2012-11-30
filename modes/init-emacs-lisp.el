;; paredit mode autopairs sexps and provides tools to manipulate sexps
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;; eldoc mode provides documentation in the minibuffer automatically
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(use-package eldoc
  :diminish eldoc-mode)

(provide 'init-emacs-lisp)
