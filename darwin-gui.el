;;; -*- lexical-binding: t -*-

;; OS X Window System configuration - fonts, meta keys etc.

;; set my favourite Mac font as the default font
(set-face-attribute 'default nil :foundry "apple"
                    :family "Inconsolata" :height 160)
;; (set-face-attribute 'default nil :foundry "adobe"
;;                     :family "Source Code Pro" :height 150)

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

;; copy SHELL correctly
(setq exec-path-from-shell-variables '("PATH" "MANPATH" "SHELL"))
;; copy shell PATH across to exec-path
(exec-path-from-shell-initialize)
