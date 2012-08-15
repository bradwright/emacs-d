;;; -*- lexical-binding: t -*-

;; OS X Window System configuration - fonts, meta keys etc.

;; set my favourite Mac font as the default font
(set-face-attribute 'default nil :foundry "apple"
                    :family "Inconsolata" :height 160)

;; This makes left-option do M-
(setq ns-alternate-modifier 'meta)
;; ... and right-option just do option.
(setq ns-right-alternate-modifier nil)

;; command is super
(setq ns-command-modifier 'super)
;; fn does nothing special for Emacs
(setq ns-function-modifier 'nil)

;; In Emacs 23 (Cocoa) in Snow Leopard, Apple delete key deletes backward, not
;; forward as is usual. This fixes this behaviour.
(normal-erase-is-backspace-mode 1)

;; Open any new buffers in the existing frame
(setq ns-pop-up-frames nil)

;; stop minimising the window accidentally
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; copy shell PATH across to exec-path
(progn
  "Get PATH from the shell, as the OSX environment is broken and weird"
  (let ((darwin-path (env-var-from-login-shell "PATH")))
    (setq exec-path (split-string darwin-path path-separator))
    (setenv "PATH" darwin-path)))
