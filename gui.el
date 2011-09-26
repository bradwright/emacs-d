;; Color themes and graphical embellishment

(require 'color-theme)

;; Theme files
(setq
 theme-dotfiles-dir
 (concat dotfiles-dir "/themes"))
(add-to-list 'load-path theme-dotfiles-dir)

(add-to-list 'load-path (concat theme-dotfiles-dir "/twilight"))
(require 'color-theme-twilight)
(color-theme-twilight)

(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(tooltip-mode -1)
(blink-cursor-mode -1)

;; stop beeping at me
(setq visible-bell t)

;; hide fringes
;;(fringe-mode 0)

;; if we do use line numbers, format them
(setq linum-format " %d ")