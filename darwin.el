;; OS X Specific configuration

;; When we have a window system, do some specific stuff
(when window-system
  (progn
    ;; Free up the option key for special characters
    (setq ns-alternate-modifer (quote none))
    (setq ns-command-modifier (quote meta))
    ;; set my favourite Mac font as the default font
    (set-face-font 'default "-apple-inconsolata-medium-r-normal--15-150-72-72-m-150-iso10646-1")))