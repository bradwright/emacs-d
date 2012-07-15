;;; -*- lexical-binding: t -*-

;; packages.el - manages packages and lists of packages
(use-package package
  :init
  (progn
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
      '(git-commit
        paredit
        yaml-mode
        scss-mode
        python-mode
        mustache-mode
        flymake-cursor
        markdown-mode
        puppet-mode
        idomenu
        ack-and-a-half))

    ;; install everything in that list
    (dolist (p packages-to-install)
      (when (not (package-installed-p p))
        (package-install p)))))
