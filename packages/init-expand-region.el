(use-package expand-region
  :config
  (progn
    (defun er/add-enh-ruby-mode-expansions ()
      "Adds Ruby-specific expansions for buffers in enh-ruby-mode"
      (set (make-local-variable 'er/try-expand-list) (append
                                                      (remove 'er/mark-defun er/try-expand-list)
                                                      '(enh-ruby-mark-defun))))

    (er/enable-mode-expansions 'enh-ruby-mode 'er/add-enh-ruby-mode-expansions))
  :bind ("C-c =" . er/expand-region))

(provide 'init-expand-region)
