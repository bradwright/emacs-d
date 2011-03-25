;; Color themes and graphical embellishment

(require 'color-theme)
(add-to-list 'load-path (concat dotfiles-dir "/vendor/themes"))
(load "twilight")
(color-theme-twilight)

(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(tooltip-mode -1)
(blink-cursor-mode -1)

;; stop beeping at me
(setq visible-bell t)