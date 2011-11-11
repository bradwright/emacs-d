;; Color themes and graphical embellishment

(require 'color-theme)

;; Theme files
(setq
 theme-dotfiles-dir
 (concat dotfiles-dir "/vendor/themes"))
(add-to-list 'load-path theme-dotfiles-dir)

;; make sure all themes are loaded
(add-to-list 'load-path (concat theme-dotfiles-dir "/twilight"))
(add-to-list 'load-path (concat theme-dotfiles-dir "/color-theme-solarized"))
(add-to-list 'load-path (concat theme-dotfiles-dir "/zenburn"))
(load-file (concat theme-dotfiles-dir "/blackboard.el"))
(require 'color-theme-twilight)
(require 'color-theme-solarized)
(require 'color-theme-zenburn)

;; use color theme
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
