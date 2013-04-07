;; show help in the echo area instead of as a tooltip
(tooltip-mode -1)

;; blink the cursor
(setq blink-cursor-interval 1.0)
(blink-cursor-mode)

;; indicate EOF empty lines in the gutter
(setq indicate-empty-lines t)

;; never pop a dialogue
(setq use-dialog-box nil)

;; From:
;; http://emacs-fu.blogspot.co.uk/2011/01/setting-frame-title.html
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(provide 'init-window-gui)
