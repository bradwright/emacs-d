;;; -*- lexical-binding: t -*-

;; Ruby mode

;; this variable is stupid - apparently Ruby needs its own indent
;; variable
;; 2-space indent is idiomatic
(setq ruby-indent-level 2)

(add-to-list 'auto-mode-alist '("[vV]agrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.css\\.erb$" . css-mode))
