(use-package css-eldoc
  :init (turn-on-css-eldoc)
  :config
  (progn
    (add-hook 'css-mode-hook 'turn-on-css-eldoc)
    (add-hook 'scss-mode-hook 'turn-on-css-eldoc)))
