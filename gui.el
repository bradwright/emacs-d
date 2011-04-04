;; Color themes and graphical embellishment

(require 'color-theme)
(add-to-list 'load-path (concat dotfiles-dir "/vendor/themes"))
(load "solarized")
(color-theme-sanityinc-solarized-dark)

(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(tooltip-mode -1)
(blink-cursor-mode -1)

;; stop beeping at me
(setq visible-bell t)