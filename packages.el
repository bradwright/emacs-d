;;; -*- lexical-binding: t -*-

;; packages.el - manages packages and lists of packages
(use-package package
  :init
  (progn
    ;; override my package directory
    (setq package-user-dir
          (file-name-as-directory (concat dotfiles-dir ".elpa/")))
    (make-directory package-user-dir t)

    ;; I use Marmalade and Melpa
    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/") t)
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (package-initialize)

    ;; stop keepalives causing weird HTTP errors
    (setq url-http-attempt-keepalives nil)

    ;; This makes heavy use of Melpa, so make sure it's installed
    (when (not (package-installed-p 'melpa))
      (progn
        (switch-to-buffer
         (url-retrieve-synchronously
          "https://raw.github.com/milkypostman/melpa/master/melpa.el"))
        (package-install-from-buffer (package-buffer-info) 'single)))

    ;;-------------------------------------------------------------------------
    ;; Patch up annoying package.el quirks
    ;;-------------------------------------------------------------------------
    ;; Gotten from: https://github.com/purcell/emacs.d/blob/master/init-elpa.el

    (defadvice package-generate-autoloads
      (after close-autoloads (name pkg-dir) activate)
      "Stop package.el from leaving open autoload files lying around."
      (let ((path (expand-file-name (concat name "-autoloads.el") pkg-dir)))
        (with-current-buffer (find-file-existing path)
          (kill-buffer nil))))

    ;; essential packages - I always install these
    (defvar essential-packages
      '(ack-and-a-half
        diminish
        expand-region
        flymake-cursor
        idomenu
        iedit
        multiple-cursors
        paredit
        smooth-scrolling
        yasnippet))

    (defun essential-packages-installed-p ()
      (loop for p in essential-packages
            when (not (package-installed-p p)) do (return nil)
            finally (return t)))

    (defun install-essential-packages ()
      (unless (essential-packages-installed-p)
        (message "%s" "Installing essential packages...")
        (package-refresh-contents)
        (dolist (p essential-packages)
          (unless (package-installed-p p)
            (package-install p)))))

    (install-essential-packages)

    ;; Only some packages we want bleeding edge
    (setq package-archive-exclude-alist
          '(("melpa"
             clojure-mode
             slime ;; slime is attached to clojure-mode
             clojure-test-mode
             haskell-mode
             idomenu
             ruby-mode
             znc)))))
