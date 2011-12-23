;; All my major and minor mode loading and configuration

;; IDO mode is awesome
(ido-mode t)
(setq
 ido-enable-prefix nil
 ido-enable-flex-matching t
 ido-create-new-buffer 'always
 ido-use-filename-at-point nil
 ido-max-prospects 10)

;; magit is awesome, always load
(add-to-list 'load-path (concat vendor-dotfiles-dir "/magit"))
(require 'magit)

;; Show colours in magit
;; force wrap commit messages
(add-hook 'magit-log-edit-mode-hook
          (lambda ()
            (setq fill-column 72)
            (turn-on-auto-fill)))

;; TODO: make all these modes a list and operate on those
(add-hook 'magit-mode-hook 'local-hl-line-mode-off)
(add-hook 'magit-log-edit-mode-hook 'local-hl-line-mode-off)

;; turn off hl-line-mode for compilation mode
(add-hook 'compilation-mode-hook 'local-hl-line-mode-off)
;; turn off hl-line-mode for shells
(add-hook 'term-mode-hook 'local-hl-line-mode-off)

;; textmate mode
(add-to-list 'load-path (concat vendor-dotfiles-dir "/textmate-mode"))
(require 'textmate)
;; Textmate mode is on for everything
(textmate-mode)

;; PHP mode
(add-to-list 'load-path (concat vendor-dotfiles-dir "/php-mode"))
(require 'php-mode)

;; Less CSS mode
(add-to-list 'load-path (concat vendor-dotfiles-dir "/less-css-mode"))
(require 'less-css-mode)

;; Undo tree
(add-to-list 'load-path (concat vendor-dotfiles-dir "/undo-tree"))
;; we don't actually want undo-tree by default...
;;(require 'undo-tree)

;; 'Evil' Vi emulation mode
(add-to-list 'load-path (concat vendor-dotfiles-dir "/evil"))
;; (require 'evil)
;; (evil-mode 1)

;; mustache templating
(add-to-list 'load-path (concat vendor-dotfiles-dir "/mustache-mode"))
(require 'mustache-mode)

(add-to-list 'load-path (concat vendor-dotfiles-dir "/haskell-mode"))
(load "haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'inferior-haskell-mode-hook 'local-hl-line-mode-off)

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

(autoload 'yaml-mode "yaml" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; Restructured text
(require 'rst)
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))
(add-hook 'rst-mode-hook
          (lambda ()
            (setq fill-column 72)
            (turn-on-auto-fill)))
(set-face-background 'rst-level-1-face nil)
(set-face-background 'rst-level-2-face nil)

;; Random missing file types
(add-to-list 'auto-mode-alist '("[vV]agrantfile$" . ruby-mode))

;; try Erlang mode
;; TODO: make this try and dynamically detect which/where Erlang
(setq
 erlang-base-dir
 "/usr/local/Cellar/erlang/R14B04")
(when (file-exists-p erlang-base-dir)
  (add-to-list 'load-path (concat erlang-base-dir "/lib/erlang/lib/tools-2.6.6.5/emacs"))
  (setq erlang-root-dir erlang-base-dir)
  (add-to-list 'exec-path (concat erlang-base-dir "/bin"))
  (setq erlang-man-root-dir (concat erlang-base-dir "/share/man"))
  (require 'erlang-start)
  ;; distel
  (add-to-list 'load-path (concat vendor-dotfiles-dir "/distel/elisp"))
  (require 'distel)
  (distel-setup))

;; ansi-term stuff
;; force ansi-term to be utf-8 after it launches
(defadvice ansi-term (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

;; since I use Magit, disable vc-mode for Git
(delete 'Git vc-handled-backends)

;; Saveplace
;;   - places cursor in the last place you edited file
(require 'saveplace)
(setq-default save-place t)
;; Keep places in the load path
(setq save-place-file (concat tmp-local-dir "/emacs-places"))

;; python-mode
(add-to-list 'load-path (concat vendor-dotfiles-dir "/python-mode"))
(require 'python-mode)

;; yasnippet
(add-to-list 'load-path (concat vendor-dotfiles-dir "/yasnippet"))
(require 'yasnippet)

(setq yas/root-directory "~/Dropbox/.emacs/yasnippets")
(yas/load-directory yas/root-directory)
(yas/initialize)

;; setup tramp mode
;; Tramp mode: allow me to SSH to hosts and edit as sudo like:
;;   C-x C-f /sudo:example.com:/etc/something-owned-by-root
;; from: http://www.gnu.org/software/tramp/#Multi_002dhops
(require 'tramp)
(setq tramp-default-method "ssh")
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))
