(use-package enh-ruby-mode
  ;; enh-ruby-mode gets regular expression step files right
  :mode (("\\.rb$" . enh-ruby-mode)
         ("[vV]agrantfile$" . enh-ruby-mode)
         ("[gG]emfile$" . enh-ruby-mode)
         ("\\.rake$" . enh-ruby-mode)
         ("\\.rabl$" . enh-ruby-mode)
         ("[cC]apfile$" . enh-ruby-mode)
         ("\\.gemspec$" . enh-ruby-mode)
         ("\\.builder$" . enh-ruby-mode))
  :config
  (progn
    ;; Ruby has a lot of camel case
    (add-hook 'enh-ruby-mode-hook 'bw-turn-on-subword-mode)
    (custom-set-faces
     '(erm-syn-errline ((t (:box nil))))
     '(erm-syn-warnline ((t (:box nil))))
     '(enh-ruby-op-face ((t (:inherit default))))
     '(enh-ruby-string-delimiter-face ((t (:foreground "#dc322f" :background nil))))
     '(enh-ruby-regexp-delimiter-face ((t (:foreground "#dc322f" :background nil))))
     '(enh-ruby-heredoc-delimiter-face ((t (:foreground "#dc322f" :background nil)))))))

(provide 'init-enh-ruby)
