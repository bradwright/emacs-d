;;; -*- lexical-binding: t -*-

;; org-mode configuration

;; Show other programming languages semi-natively when embedded
(org-babel-do-load-languages
 'org-babel-load-languages
 ;; load emacs-lisp natively
 '((emacs-lisp . t)
   (ruby . t)))

;; edit inline code blocks natively
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; auto-wrap
(add-hook 'org-mode-hook 'bw-turn-on-auto-fill)
(add-hook 'org-mode-hook 'bw-turn-off-trailing-whitespace)

;; default directory
(setq org-directory "~/Dropbox/Org/")
