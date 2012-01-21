;; Color themes and graphical embellishment

;; Theme files
(setq
 theme-dotfiles-dir
 (concat dotfiles-dir "/vendor/themes"))
(add-to-list 'load-path theme-dotfiles-dir)

;; make sure all themes are loaded
;; no themes right now

(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(tooltip-mode -1)
(blink-cursor-mode -1)

;; stop beeping at me
(setq visible-bell t)

;; if we do use line numbers, format them
(setq linum-format " %d ")
