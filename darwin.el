;; OS X Specific configuration

;; Free up the option key for special characters
(when window-system
  (setq ns-alternate-modifer (quote none)))
(when window-system
  (setq ns-command-modifier (quote meta)))
;; set my favourite Mac font as the default font
(when window-system
  (set-face-font 'default "-apple-inconsolata-medium-r-normal--15-150-72-72-m-150-iso10646-1"))