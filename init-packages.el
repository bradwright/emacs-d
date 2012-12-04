(use-package package
  :init
  (progn
    ;; override my package directory
    (defconst package-base-dir
      (bw-join-dirs dotfiles-dir "elpa"))
    (defconst package-install-dir
      (bw-join-dirs package-base-dir "installed"))

    (make-directory package-base-dir t)
    (make-directory package-install-dir t)

    (bw-add-to-load-path package-base-dir)

    ;; this is to set up packages
    (setq package-user-dir package-install-dir)

    ;; I use Marmalade and Melpa
    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/") t)
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (package-initialize)

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
        browse-kill-ring
        diminish
        expand-region
        flymake-cursor
        git-commit-mode
        idomenu
        iedit
        multiple-cursors
        paredit
        smex
        undo-tree
        yasnippet)
      "Packages that I always use")

    (if *is-a-mac*
        (add-to-list 'essential-packages 'exec-path-from-shell))

    (defun essential-packages-installed-p ()
      (loop for p in essential-packages
            when (not (package-installed-p p)) do (return nil)
            finally (return t)))

    (defun maybe-package-refresh-contents ()
      "Conditionally refreshes package archive"
      (interactive)
      (unless
          (and
           (file-exists-p (concat package-user-dir "archives/marmalade"))
           (file-exists-p (concat package-user-dir "archives/gnu"))
           (file-exists-p (concat package-user-dir "archives/melpa")))
        (package-refresh-contents)))

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
             multiple-cursors
             ruby-mode
             undo-tree
             znc)))))

(provide 'init-packages)
