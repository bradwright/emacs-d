(use-package paredit
  :config
  (progn
    ;; change keyboard commands only in terminal mode
    (when (not (display-graphic-p))
      (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
      (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp))))

(provide 'init-paredit)
