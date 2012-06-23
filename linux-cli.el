;;; -*- lexical-binding: t -*-

;;; Linux CLI changes

;; Conditional colour theme loading
(if (eq emacs-major-version 24)
    (load-theme 'tango-dark t))
