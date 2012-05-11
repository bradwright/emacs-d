;; packages.el - manages packages and lists of packages

;; conditional for the O.G emacs
;;(when (not (require 'package nil t))
;;  (require 'package "package-23.el"))

(require 'package)

;; override my package directory
(setq package-user-dir (concat dotfiles-dir "/.elpa"))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; some packages we want to be boring and just use Marmalade
(setq package-archive-exclude-alist
      '(("melpa" clojure-mode clojure-test-mode haskell-mode slime paredit)))

;; make sure we're up to date with archives
(when (null package-archive-contents)
  (package-refresh-contents))

(defvar packages-to-install
  '(melpa
    clojure-mode
    clojure-test-mode
    haskell-mode
    textmate
    paredit
    yaml-mode
    less-css-mode
    php-mode
    mustache-mode
    magit
    flymake-cursor
    markdown-mode
    coffee-mode
    gist
    puppet-mode
    idomenu
    find-file-in-project
    znc))

;; install everything in that list
(dolist (p packages-to-install)
  (when (not (package-installed-p p))
    (package-install p)))
