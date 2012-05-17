;; packages.el - manages packages and lists of packages

;; conditional for the O.G emacs
(when (not (require 'package nil t))
  (require 'package "vendor/package-23.el"))

;; override my package directory
(setq package-user-dir (concat dotfiles-dir "/.elpa"))

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
  '(clojure-mode
    clojure-test-mode
    haskell-mode
    textmate
    paredit
    js2-mode
    yaml-mode
    less-css-mode
    php-mode
    python-mode
    mustache-mode
    magit
    flymake-cursor
    markdown-mode
    coffee-mode
    puppet-mode
    idomenu
    find-file-in-project
    znc
    color-theme-solarized))

;; install everything in that list
(dolist (p packages-to-install)
  (when (not (package-installed-p p))
    (package-install p)))
