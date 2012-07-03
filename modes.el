;;; -*- lexical-binding: t -*-

;; All my major and minor mode loading and configuration

;; IDO mode is awesome
(ido-mode t)
(setq
 ido-enable-prefix nil
 ido-enable-flex-matching t
 ido-create-new-buffer 'always
 ido-use-filename-at-point nil
 ido-max-prospects 20
 ido-case-fold t)

;;; Magit
(use-package magit
  :bind ("C-c g" . magit-status)
  :config
  (progn
    ;; force wrap magit commit messages
    (eval-after-load 'magit '(progn
                               (add-hook 'magit-log-edit-mode-hook 'bw-turn-on-auto-fill)
                               (add-hook 'magit-log-edit-mode-hook 'bw-fill-column)
                               (add-hook 'git-commit-mode-hook 'bw-turn-on-auto-fill)
                               (add-hook 'git-commit-mode-hook 'bw-fill-column)))

    ;; TODO: make all these modes a list and operate on those
    (add-hook 'magit-mode-hook 'local-hl-line-mode-off)
    (add-hook 'magit-log-edit-mode-hook 'local-hl-line-mode-off)
    ;; magit extensions
    ;; FIXME: do I even need this?
    (require 'magit-blame)

    ;; magit settings
    (setq
     ;; use ido to look for branches
     magit-completing-read-function 'magit-ido-completing-read
     ;; don't put "origin-" in front of new branch names by default
     magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
     ;; open magit status in same window as current buffer
     magit-status-buffer-switch-function 'switch-to-buffer
     ;; highlight word/letter changes in hunk diffs
     magit-diff-refine-hunk t)))

;; turn off hl-line-mode for compilation mode
(add-hook 'compilation-mode-hook 'local-hl-line-mode-off)
;; turn off hl-line-mode for shells
(add-hook 'term-mode-hook 'local-hl-line-mode-off)

;; haskell mode, loaded via Elpa
(when (fboundp 'haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'inferior-haskell-mode-hook 'local-hl-line-mode-off))

;; Jinja mode is a bit crap, really
(require 'jinja)
(add-to-list 'auto-mode-alist '("\\.jinja$" . jinja-mode))

;; JSON files
(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

;; load yaml files correctly
;; yaml-mode doesn't auto-load for some reason
(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; rst-mode isn't always around in HEAD Emacs
(ignore-errors (require 'rst nil t)
  (add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))
  (add-hook 'rst-mode-hook 'bw-turn-on-auto-fill)
  ;; kill stupid heading faces
  (set-face-background 'rst-level-1-face nil)
  (set-face-background 'rst-level-2-face nil))

;; Puppet manifests
(use-package puppet-mode
  :mode ("\\.pp$" . puppet-mode))

;; ansi-term stuff
;; force ansi-term to be utf-8 after it launches
(defadvice ansi-term
  (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

(when (require 'ansi-color nil t)
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;; since I use Magit, disable vc-mode for Git
(delete 'Git vc-handled-backends)

;; Saveplace
;;   - places cursor in the last place you edited file
(use-package saveplace
  :init
  (progn
    (setq-default save-place t)
    ;; Keep places in the load path
    (setq save-place-file (file-name-as-directory (concat tmp-local-dir "emacs-places")))))

;; setup tramp mode
;; Tramp mode: allow me to SSH to hosts and edit as sudo like:
;;   C-x C-f /sudo:example.com:/etc/something-owned-by-root
;; from: http://www.gnu.org/software/tramp/#Multi_002dhops
(use-package tramp
  :init
  (progn
    (setq tramp-default-method "ssh")
    (add-to-list 'tramp-default-proxies-alist
                 '(nil "\\`root\\'" "/ssh:%h:"))
    (add-to-list 'tramp-default-proxies-alist
                 '((regexp-quote (system-name)) nil nil))))

;; Clojure mode, installed via Elpa
(use-package clojure-mode
  :mode ("\\.clj\\'" . clojure-mode)
  :config
  (progn
      (add-hook 'clojure-mode-hook 'turn-on-paredit)
      (add-hook 'clojure-mode-hook 'bw-clojure-repl-program)
      (add-hook 'slime-repl-mode-hook 'bw-clojure-slime-repl-font-lock)
      (add-hook 'slime-repl-mode-hook 'local-hl-line-mode-off)
      (add-hook 'slime-repl-mode-hook 'turn-on-paredit)))

;; load Flymake cursor
(when (load "flymake" t)
  (require 'flymake-cursor))

;; new python-mode IDE
(use-package python-mode
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

;; http://stackoverflow.com/a/4485083/61435
;; Automatically save and restore sessions
(setq desktop-dirname (file-name-as-directory (concat tmp-local-dir "desktops"))
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-load-locked-desktop nil)
(desktop-save-mode 1)

;; ediff
(setq ediff-split-window-function 'split-window-horizontally
      ediff-diff-options          "-w"
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; eproject mode
(use-package eproject
  :load-path "vendor/eproject-mode/lang")

(add-hook 'after-init-hook 'bw-load-mode-files)

;; include YAML in find-file-in-project
(use-package find-file-in-project
  :config
  (progn
    (add-to-list 'ffip-patterns "*.yml")
    (add-to-list 'ffip-patterns "*.css")
    (add-to-list 'ffip-patterns "*.scss")
    (add-to-list 'ffip-patterns "*.less")
    (add-to-list 'ffip-patterns "*.rst")))

;; coffee script and iced coffee
(use-package coffee-mode
  :mode (("\\.coffee\'" . coffee-mode)
         ("\\.iced\'" . coffee-mode))
  :config
  (progn
    (make-local-variable 'tab-width)
    (set 'tab-width 2)

    ;; Compile '.coffee' files on every save
    (and (file-exists-p (buffer-file-name))
         (file-exists-p (coffee-compiled-file-name))
         (coffee-cos-mode t)))))
