;; Packaging, el-get, and vendor configuration

(defconst bw-package-base-dir
  (bw-join-dirs dotfiles-dir "packages")
  "Base path for all packaging stuff")

(bw-add-to-load-path bw-package-base-dir)

;; package lists
;; As a general rule, always install from ELPA unless there's a
;; specific reason to get it from el-get.

;; Packages I always want
(defvar bw-elpa-package-list
  '(ack-and-a-half
    browse-kill-ring
    csv-mode
    diminish
    enh-ruby-mode
    eproject
    expand-region
    flymake-cursor
    git-commit-mode
    gitconfig-mode
    gitignore-mode
    ido-ubiquitous
    idomenu
    js2-mode
    json-mode
    markdown-mode
    multiple-cursors
    paredit
    puppet-mode
    scss-mode
    smex
    undo-tree
    web-mode)
  "Packages from ELPA that I always want to install.")

;; Mac specific packages
(when *is-a-mac*
  (add-to-list 'bw-elpa-package-list 'edit-server)
  (add-to-list 'bw-elpa-package-list 'exec-path-from-shell))

;; Whitelist melpa packages, and provide reasons they're whitelisted.
(setq package-archive-enable-alist
      '(("melpa"
         ack-menu             ;; Only in MELPA
         browse-kill-ring     ;; Marmalade version is very old
         carton               ;; Only in MELPA
         csv-mode             ;; need more up to date version
         edit-server          ;; Only in MELPA
         enh-ruby-mode        ;; Only in MELPA
         eproject             ;; Only in MELPA
         exec-path-from-shell ;; Marmalade version is very old
         expand-region        ;; Marmalade version is very old
         git-commit-mode      ;; Only in MELPA
         gitconfig-mode       ;; Only in MELPA
         gitignore-mode       ;; Only in MELPA
         js2-mode             ;; need latest version
         multiple-cursors     ;; Melpa version is more up to date
         pallet               ;; Only in MELPA
         rinari               ;; Marmalade version is very old
         web-mode             ;; Only in MELPA
         yaml-mode            ;; Marmalade version is very old
         )))

;; Packages to exclude from specific repos, and reasons for excluding
;; them.
(setq package-archive-exclude-alist
      '(("melpa"
         ruby-mode ;; they have ruby-mode 1.1, which is out of sync
                   ;; with Emacs.
         )))

;; el-get packages to install, and reasons for wanting them.
(defvar bw-el-get-package-list
  '(use-package     ;; not on elpa
    el-get          ;; self-hosting
    magit           ;; Want the info files Make gives you
    solarized-theme ;; Want specific branch on GitHub
    xterm-frobs     ;; not on Elpa
    )
  "Packages from el-get that I always want to install.")

;;; ELPA directory structure and loading
(eval-after-load 'package
  '(progn
     (defvar bw-package-elpa-base-dir
       (bw-join-dirs bw-package-base-dir "elpa")
       "Where Emacs packaging.el packages are installed.")

     (make-directory bw-package-elpa-base-dir t)

     ;; tell packaging to install files here
     (setq package-user-dir bw-package-elpa-base-dir)

     ;; Only use 3 specific directories
     (setq package-archives
           '(("gnu"       . "http://elpa.gnu.org/packages/")
             ("marmalade" . "http://marmalade-repo.org/packages/")
             ("melpa"     . "http://melpa.milkbox.net/packages/")))

     ;; initialise package.el
     (package-initialize)

     ;; Clean up after ELPA installs:
     ;; https://github.com/purcell/emacs.d/blob/master/init-elpa.el
     (defadvice package-generate-autoloads
       (after close-autoloads (name pkg-dir) activate)
       "Stop package.el from leaving open autoload files lying around."
       (let ((path (expand-file-name (concat name "-autoloads.el") pkg-dir)))
         (with-current-buffer (find-file-existing path)
           (kill-buffer nil))))

     ;; Auto-install the Melpa package, since it's used to filter
     ;; packages.
     (when (not (package-installed-p 'melpa))
       (progn
         (switch-to-buffer
          (url-retrieve-synchronously
           "https://raw.github.com/milkypostman/melpa/master/melpa.el"))
         (package-install-from-buffer (package-buffer-info) 'single)))

     (defun essential-packages-installed-p ()
       "Checks whether all my essential packages are installed."
       (loop for p in bw-elpa-package-list
             when (not (package-installed-p p)) do (return nil)
             finally (return t)))

     (defun install-essential-packages ()
       "Auto-installs all my packages"
       (unless (essential-packages-installed-p)
         (message "%s" "Installing essential packages...")
         (package-refresh-contents)
         (dolist (p bw-elpa-package-list)
           (unless (package-installed-p p)
             (package-install p)))))

     (install-essential-packages)))

;; el-get directory structure
(defconst bw-el-get-base-dir
  (bw-join-dirs bw-package-base-dir "el-get"))

(defconst bw-el-get-installed-dir
  (bw-join-dirs bw-el-get-base-dir "installed"))

(setq el-get-dir bw-el-get-installed-dir)

;; if el-get is already installed it'll live here
(bw-add-to-load-path (bw-join-dirs bw-el-get-installed-dir "el-get"))

(eval-after-load 'el-get
  '(progn
     ;; all stored recipes are here
     (add-to-list 'el-get-recipe-path (bw-join-dirs bw-el-get-base-dir "recipes"))
     ;; only take the latest commit and not the whole history
     (setq el-get-git-shallow-clone t)))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let ((el-get-master-branch)
          ;; skip EmacsWiki recipes
          (el-get-install-skip-emacswiki-recipes))
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; It should have been installed by now - initialise
(el-get 'sync bw-el-get-package-list)

(provide 'init-packaging)
