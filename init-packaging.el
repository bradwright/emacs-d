;; Packaging and vendor configuration

(defconst bw-package-base-dir
  (bw-join-dirs dotfiles-dir "packages")
  "Base path for all packaging stuff")

(bw-add-to-load-path bw-package-base-dir)

;; package lists

;; Packages I always want
(defvar bw-elpa-package-list
  '(ack-and-a-half
    bind-key ;; this is required by use-package, but isn't auto-installed
    browse-kill-ring
    csv-mode
    diminish
    enh-ruby-mode
    eproject
    evil
    expand-region
    flx
    flymake-cursor
    flymake-go
    git-commit-mode
    gitconfig-mode
    gitignore-mode
    go-mode
    ido-ubiquitous
    ido-vertical-mode
    idomenu
    js2-mode
    json-mode
    magit
    markdown-mode
    multiple-cursors
    paredit
    puppet-mode
    scss-mode
    smex
    undo-tree
    use-package
    web-mode
    xterm-frobs)
  "Packages from ELPA that I always want to install.")

;; Mac specific packages
(when *is-a-mac*
  (add-to-list 'bw-elpa-package-list 'edit-server)
  (add-to-list 'bw-elpa-package-list 'exec-path-from-shell))

;; Blacklist some non-melpa packages
(setq package-archive-exclude-alist
      '(("melpa"
         ack-and-a-half  ;; not updated in ages
         diminish        ;; not updated in ages
         evil            ;; want stable version
         flymake-cursor  ;; Melpa version is on wiki
         idomenu         ;; not updated in ages
         json-mode       ;; not on Melpa
         melpa           ;; don't want to self-host this
         )))

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

(require 'package nil t)

(provide 'init-packaging)
