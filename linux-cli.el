;;; -*- lexical-binding: t -*-

;;; Linux CLI changes

;; Conditional colour theme loading
(if (eq emacs-major-version 24)
    (load-theme 'solarized-dark t)
  (when (fboundp 'color-theme)
    (color-theme-solarized-dark)))

;; Enable the mouse, gotten from:
;; http://www.iterm2.com/#/section/faq
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
