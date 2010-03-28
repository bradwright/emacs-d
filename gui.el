;; Color themes and graphical embellishment

(require 'color-theme)
(setq color-theme-is-global t)
(add-to-list 'load-path "~/.emacs-d/dist/elisp")
(load "color-theme-twilight")
(color-theme-twilight)

(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(tooltip-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)