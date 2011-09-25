;; All my major and minor mode loading and configuration

;; IDO mode is awesome
(ido-mode t)
(setq
 ido-enable-prefix nil
 ido-enable-flex-matching t
 ido-create-new-buffer 'always
 ido-use-filename-at-point nil
 ido-max-prospects 10)

;; vendor files
(setq
 vendor-dotfiles-dir
 (concat dotfiles-dir "/vendor"))
(add-to-list 'load-path vendor-dotfiles-dir)

;; magit is awesome, always load
(add-to-list 'load-path (concat vendor-dotfiles-dir "/magit"))
(require 'magit)

;; Show colours in magit
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))
;; force wrap commit messages
(add-hook 'magit-log-edit-mode-hook
          (lambda ()
            (setq fill-column 72)
            (turn-on-auto-fill)))

;; textmate mode
(add-to-list 'load-path (concat vendor-dotfiles-dir "/textmate-mode"))
(require 'textmate)
;; Textmate mode is on for everything
(textmate-mode)

;; PHP mode
(add-to-list 'load-path (concat vendor-dotfiles-dir "/php-mode"))
(require 'php-mode)

;; Undo tree
(add-to-list 'load-path (concat vendor-dotfiles-dir "/undo-tree"))
(require 'undo-tree)

;; 'Evil' Vi emulation mode
(add-to-list 'load-path (concat vendor-dotfiles-dir "/evil"))
(require 'evil)
;; (evil-mode 1)

(add-to-list 'load-path (concat vendor-dotfiles-dir "/mustache-mode"))
(require 'mustache-mode)

;; JS2 mode, not espresso
(add-to-list 'load-path (concat vendor-dotfiles-dir "/js2-mode"))
(require 'js2-mode)
(setq
 js2-highlight-level 3
 js2-basic-offset 4
 js2-consistent-level-indent-inner-bracket-p t
 js2-pretty-multiline-decl-indentation-p t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Jinja mode is a bit crap, really
(require 'jinja)
(add-to-list 'auto-mode-alist '("\\.jinja$" . jinja-mode))

;; JSON files
(add-to-list 'load-path (concat vendor-dotfiles-dir "/json-mode"))
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(defun my-html-mode-hook ()
  (setq tab-width 4)
  (auto-fill-mode 0)
  (define-key html-mode-map (kbd "<tab>") 'my-insert-tab)
  (define-key html-mode-map (kbd "C->") 'sgml-close-tag))

;; just insert tabs
(defun my-insert-tab (&optional arg)
  (interactive "P")
  (insert-tab arg))

(add-hook 'html-mode-hook 'my-html-mode-hook)

;; nXhtml - which I don't use
;;(load "nxhtml/autostart.el")

(autoload 'yaml-mode "yaml" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; Restructured text
(require 'rst)
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))

;; Random missing file tyoes
(add-to-list 'auto-mode-alist '("[vV]agrantfile$" . ruby-mode))