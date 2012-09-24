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
     ido-max-prospects 20
     ido-case-fold t)))

;;; Magit
(use-package magit
  :bind ("C-c g" . magit-status)
  :init
  (progn
    (add-to-list 'Info-default-directory-list (bw-locate-library-dir "magit"))
    (delete 'Git vc-handled-backends))
  :config
  (progn
    ;; force wrap magit commit messages
    (add-hook 'magit-log-edit-mode-hook 'bw-turn-on-auto-fill)
    (add-hook 'magit-log-edit-mode-hook 'bw-fill-column)
    (add-hook 'git-commit-mode-hook 'bw-turn-on-auto-fill)
    (add-hook 'git-commit-mode-hook 'bw-fill-column)


    (add-hook 'magit-mode-hook 'local-hl-line-mode-off)
    (add-hook 'magit-log-edit-mode-hook 'local-hl-line-mode-off)
    ;; magit extensions
    (use-package magit-blame
      :bind ("C-c C-b" . magit-blame-mode))
    (use-package rebase-mode)

    ;; magit settings
    (setq
     ;; use ido to look for branches
     magit-completing-read-function 'magit-ido-completing-read
     ;; don't put "origin-" in front of new branch names by default
     magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
     ;; open magit status in same window as current buffer
     magit-status-buffer-switch-function 'switch-to-buffer
     ;; highlight word/letter changes in hunk diffs
     magit-diff-refine-hunk t
     ;; ask me if I want to include a revision when rewriting
     magit-rewrite-inclusive 'ask
     ;; don't always save buffers
     magit-save-some-buffers nil)))

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
  :mode ("\\.ya?ml\\'" . yaml-mode))

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

;; eproject mode
(use-package eproject
  :diminish eproject-mode
  :config
  (progn
    ;; ruby on rails special type
    (define-project-type ruby-on-rails-git (generic-git)
      (and (look-for ".git")
           (look-for "Gemfile")
           (look-for "config/application.rb"))
      :main-file "Gemfile")

    (defun rails-eproject-test (&optional only-this-file)
      "Runs local tests"
      (interactive "P")
      (if (eq only-this-file nil)
          (compile "bundle exec rake" t)
        (compile (concat "bundle exec ruby -I test " (buffer-file-name)) t)))

    (defun rails-eproject-hook ()
      "Set up some local variables"
      (add-to-list 'safe-local-variable-values '(scss-sass-command . t))

      ;; (shell-command "git ls-files | /usr/local/bin/ctags -e -f - -L - 1> TAGS 2> /dev/null")

      (set (make-local-variable 'inf-ruby-default-implementation) "bundle-ruby")

      ;; run rake to compile
      (set (make-local-variable 'compile-command) "bundle exec rake")
      (local-set-key (kbd "C-c C-t") 'rails-eproject-test))

    (add-hook 'ruby-on-rails-git-project-file-visit-hook 'rails-eproject-hook)

    (defun bw-eproject-find-files ()
      "If we're in a Git project, use git ls-files to look up the
files, because it won't try to open any .gitignored files."
      (interactive)
      (if (member (eproject-type) '(generic-git ruby-on-rails-git))
          (let ((default-directory (eproject-root)))
            (find-file
             (concat
              (eproject-root)
              (ido-completing-read
               (format "Find file: %s" (eproject-root))
               (split-string (shell-command-to-string "git ls-files --exclude-standard -co"))))))
        (eproject-find-file)))
    (use-package eproject-extras
      :bind ("C-c f" . bw-eproject-find-files))
    (setq eproject-completing-read-function 'eproject--ido-completing-read)))

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

(use-package rhtml-mode
  :mode ("\\.html\\.erb\\'" . rhtml-mode))

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
    (add-to-list 'safe-local-variable-values '(ack-and-a-half-arguments . ("--nopager"))))
  :bind ("C-c C-f" . ack-and-a-half)
  :config
  (progn
    (add-to-list 'ack-and-a-half-arguments "--nopager")
    (setq ack-and-a-half-prompt-for-directory t)
    (add-hook 'eproject-first-buffer-hook (lambda ()
                                            (when (eproject-root)
      (set (make-local-variable 'ack-and-a-half-root-directory-functions) 'ack-and-a-half-root-directory-functions)
      (add-to-list 'ack-and-a-half-root-directory-functions 'eproject-root))))))

(use-package git-commit
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
  :bind ("C-=" . er/expand-region))

;; multiple cursors
(use-package multiple-cursors
  :bind (("C-c >" . mc/mark-next-like-this)
         ("C-c <" . mc/mark-previous-like-this)
         ("C-c C-c <" . mc/mark-all-like-this)))
