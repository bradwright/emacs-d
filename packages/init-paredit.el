(use-package paredit
  :config
  (progn
    ;; change keyboard commands only in terminal mode
    (when (not (display-graphic-p))
      (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
      (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp))

    ;; Enable `paredit-mode' in the minibuffer, during `eval-expression'.
    (defun conditionally-enable-paredit-mode ()
      (if (eq this-command 'eval-expression)
          (paredit-mode 1)))

    (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)))

(provide 'init-paredit)
