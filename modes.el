;;; -*- lexical-binding: t -*-

;; All my major and minor mode loading and configuration

;; defer loading of separate mode configuration
(add-hook 'after-init-hook 'bw-load-mode-files)

;; IDO mode is awesome
(use-package ido
  :init (ido-mode t)
  :config
  (progn
    (setq
     ido-enable-prefix nil
     ido-enable-flex-matching t
     ido-create-new-buffer 'always
     ido-use-filename-at-point nil
     ido-case-fold t)))

;; turn off hl-line-mode for compilation mode
(add-hook 'compilation-mode-hook 'local-hl-line-mode-off)
;; turn off hl-line-mode for shells
(add-hook 'term-mode-hook 'local-hl-line-mode-off)

;; haskell mode, loaded via Elpa
(use-package haskell-mode
  :commands haskell-mode
  :mode ("\\.l?hs$" . haskell-mode)
  :config
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'inferior-haskell-mode-hook 'local-hl-line-mode-off)))

;; JSON files
(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

;; load yaml files correctly
;; yaml-mode doesn't auto-load for some reason
(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :config
  (progn
    (put 'yaml-indent-offset 'safe-local-variable 'integerp)))

;; rst-mode isn't always around in HEAD Emacs
(use-package rst
  :mode ("\\.rst$" . rst-mode)
  :config
  (progn
    (add-hook 'rst-mode-hook 'bw-turn-on-auto-fill)
    ;; kill stupid heading faces
    (set-face-background 'rst-level-1-face nil)
    (set-face-background 'rst-level-2-face nil)))

;; Puppet manifests
(use-package puppet-mode
  :mode ("\\.pp$" . puppet-mode))

;; ansi-term stuff
;; force ansi-term to be utf-8 after it launches
(defadvice ansi-term
  (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

(use-package ansi-color
  :config
  (progn
    (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)))

;; Saveplace
;;   - places cursor in the last place you edited file
(use-package saveplace
  :config
  (progn
    (setq-default save-place t)
    ;; Keep places in the load path
    (setq save-place-file (file-name-as-directory (concat tmp-local-dir "emacs-places")))))

;; setup tramp mode
;; Tramp mode: allow me to SSH to hosts and edit as sudo like:
;;   C-x C-f /sudo:example.com:/etc/something-owned-by-root
;; from: http://www.gnu.org/software/tramp/#Multi_002dhops
(use-package tramp
  :config
  (progn
    (setq tramp-default-method "ssh")
    (add-to-list 'tramp-default-proxies-alist
                 '(nil "\\`root\\'" "/ssh:%h:"))
    (add-to-list 'tramp-default-proxies-alist
                 '((regexp-quote (system-name)) nil nil))))

;; Clojure mode, installed via Elpa
(use-package clojure-mode
  :config
  (progn
      (add-hook 'clojure-mode-hook 'turn-on-paredit)
      (add-hook 'clojure-mode-hook 'bw-clojure-repl-program)
      (add-hook 'slime-repl-mode-hook 'bw-clojure-slime-repl-font-lock)
      (add-hook 'slime-repl-mode-hook 'local-hl-line-mode-off)
      (add-hook 'slime-repl-mode-hook 'turn-on-paredit)))

;; load Flymake cursor
(use-package flymake
  :config
  (progn
    (use-package flymake-cursor)))

;; new python-mode IDE
(use-package python-mode
  :mode ("\\.py$" . python-mode)
  :config
  (progn
    (setq py-install-directory (concat vendor-dotfiles-dir "python-mode"))
    (setq py-start-run-py-shell nil)))

;; markdown
(use-package markdown-mode
  :mode (("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)))

;; PHP - why doesn't PHP-mode do this already?
(use-package php-mode
  :mode ("\\.php$" . php-mode))

;; emacs lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)

;; eshell
(eval-after-load 'esh-opt
  '(progn
     ;; we need this to override visual commands
     (require 'em-term)
     ;; If I try to SSH from an eshell, launch it in ansi-term instead
     (add-to-list 'eshell-visual-commands "ssh")))
;; fix ANSI colour issues from test runners etc.
(add-hook 'eshell-preoutput-filter-functions
          'ansi-color-filter-apply)

;; ediff
(use-package ediff
  :config
  (progn
    (setq ediff-split-window-function 'split-window-horizontally
          ediff-diff-options          "-w"
          ediff-window-setup-function 'ediff-setup-windows-plain)))

;; coffee script and iced coffee
(use-package coffee-mode
  :mode (("\\.coffee\\'" . coffee-mode)
         ("\\.iced\\'" . coffee-mode))
  :config
  (progn
    (set (make-local-variable 'tab-width) 2)

    ;; Compile '.coffee' files on every save
    (and (file-exists-p (buffer-file-name))
         (file-exists-p (coffee-compiled-file-name))
         (coffee-cos-mode t))))

(use-package web-mode
  :mode ("\\.html\\.erb\\'" . web-mode))

(use-package paredit
  :config
  (progn
    ;; change keyboard commands only in terminal mode
    (when (not (display-graphic-p))
      (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
      (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp))))

(use-package ack-and-a-half
  :init
  (progn
    (put 'ack-and-a-half-arguments 'safe-local-variable 'listp))
  :bind ("C-c C-a" . ack-and-a-half)
  :config
  (progn
    (add-to-list 'ack-and-a-half-arguments "--nopager")
    (setq ack-and-a-half-prompt-for-directory t)
    (add-hook 'eproject-first-buffer-hook (lambda ()
                                            (when (eproject-root)
      (set (make-local-variable 'ack-and-a-half-root-directory-functions) 'ack-and-a-half-root-directory-functions)
      (add-to-list 'ack-and-a-half-root-directory-functions 'eproject-root))))))

(use-package git-commit-mode
  :mode (("COMMIT_EDITMSG" . git-commit-mode)
         ("NOTES_EDITMSG" . git-commit-mode)
         ("MERGE_MSG" . git-commit-mode)
         ("TAG_EDITMSG" . git-commit-mode)))

(use-package whitespace
  :init
  :diminish whitespace-mode
  (progn
    ;; display only tails of lines longer than 80 columns, tabs and
    ;; trailing whitespaces
    ;; style information is here: http://www.emacswiki.org/emacs/WhiteSpace
    (setq whitespace-line-column 80
          whitespace-style '(face tabs trailing lines-tail))

    (global-whitespace-mode t)

    (setq whitespace-global-modes '(not erc-mode))))

(use-package abbrev
  :diminish abbrev-mode)

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package uniquify
  :config
  (progn
    ;; this shows foo/bar and baz/bar when two files are named bar
    (setq uniquify-buffer-name-style 'forward)
    ;; strip common suffixes
    (setq uniquify-strip-common-suffix t)))

;; always start a server
(use-package server
  :init
  (progn
    (unless (server-running-p)
      (server-start))))

;; expand-region
(use-package expand-region
  :bind ("C-c =" . er/expand-region))


(use-package iedit
  :bind (("C-c ;" . iedit-mode)))

;; multiple cursors
(use-package multiple-cursors
  :bind (("C-c ." . mc/mark-next-like-this)
         ("C-c ," . mc/mark-previous-like-this)
         ("C-c C-l". mc/mark-all-like-this)))

(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring))

;; make hippie expand behave itself
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (progn
    (yas-global-mode 1)
    (setq yas/prompt-functions '(yas/ido-prompt yas/completing-prompt))))

;; set the title on xterm
(use-package xterm-frobs
  :init
  (progn
    (unless (display-graphic-p)
      (defun bw-xterm-title ()
        (xterm-set-window-title (concat "emacs@" (system-name)))
        (xterm-set-icon-title (buffer-name)))
      (add-hook 'window-configuration-change-hook 'bw-xterm-title))))
