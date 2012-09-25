;;; -*- lexical-binding: t -*-

;; packages.el - manages packages and lists of packages
(use-package package
  :init
  (progn
    ;;------------------------------------------------------------------------------
    ;; Patch up annoying package.el quirks
    ;;------------------------------------------------------------------------------
    ;; Gotten from: https://github.com/purcell/emacs.d/blob/master/init-elpa.el

    (defadvice package-generate-autoloads (after close-autoloads (name pkg-dir) activate)
      "Stop package.el from leaving open autoload files lying around."
      (let ((path (expand-file-name (concat name "-autoloads.el") pkg-dir)))
        (with-current-buffer (find-file-existing path)
          (kill-buffer nil))))

    ;; override my package directory
    (setq package-user-dir (file-name-as-directory (concat dotfiles-dir ".elpa/")))
    (make-directory package-user-dir t)

    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/") t)
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.milkbox.net/packages/") t)

    (package-initialize)

    ;; Only some packages we want bleeding edge
    (setq package-archive-exclude-alist
          '(("melpa"
             clojure-mode
             slime ;; slime is attached to clojure-mode
             clojure-test-mode
             haskell-mode
             idomenu
             ruby-mode
             znc)))

    ;; refresh archives
    (when (null package-archive-contents)
      (package-refresh-contents))

    ;; install melpa first so we can filter packages
    (when (not (package-installed-p 'melpa))
      (package-install 'melpa)
      (package-refresh-contents))

    ;; packages I install
    (defvar packages-to-install
      '(paredit
        diminish
        ace-jump-mode
        yaml-mode
        scss-mode
        python-mode
        mustache-mode
        flymake-cursor
        markdown-mode
        puppet-mode
        idomenu
        ack-and-a-half
        inf-ruby
        expand-region
        multiple-cursors
        browse-kill-ring))

    ;; install everything in that list
    (dolist (p packages-to-install)
      (when (not (package-installed-p p))
        (package-install p)))))
