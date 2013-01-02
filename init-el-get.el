;; el-get configuration

(defconst package-base-dir
  (bw-join-dirs dotfiles-dir "packages"))

(make-directory package-base-dir t)
(bw-add-to-load-path package-base-dir)

(defconst el-get-base-dir
  (bw-join-dirs package-base-dir "el-get"))

(make-directory el-get-base-dir t)

(defconst el-get-installed-dir
  (bw-join-dirs el-get-base-dir "installed"))

(setq el-get-dir el-get-installed-dir)

;; if el-get is already installed it'll live here
(bw-add-to-load-path (bw-join-dirs el-get-installed-dir "el-get"))

;; override package recipe - the built-in el-get one has weird archive settings
(setq el-get-sources
      '((:name package
               :post-init
               (progn
                 ;; Gotten from:
                 ;; https://github.com/purcell/emacs.d/blob/master/init-elpa.el
                 (defadvice package-generate-autoloads
                   (after close-autoloads (name pkg-dir) activate)
                   "Stop package.el from leaving open autoload files lying around."
                   (let ((path (expand-file-name (concat name "-autoloads.el") pkg-dir)))
                     (with-current-buffer (find-file-existing path)
                       (kill-buffer nil))))

                 (setq package-archives
                       '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
                 (defconst package-install-dir
                   (bw-join-dirs package-base-dir "elpa"))

                 (make-directory package-install-dir t)
                 ;; this is to set up packages
                 (setq package-user-dir package-install-dir)))))

(eval-after-load 'el-get
  '(progn
     ;; all stored recipes are here
     (add-to-list 'el-get-recipe-path (bw-join-dirs el-get-base-dir "recipes"))
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

(setq bw-packages
      '(use-package ; this has to come first due to the way my config
                    ; is set up
         ack-and-a-half
         browse-kill-ring
         color-theme-solarized
         csv-mode
         diminish
         el-get
         eproject
         expand-region
         flymake-cursor
         git-modes
         ido-ubiquitous
         idomenu
         js2-mode
         json-mode
         magit
         markdown-mode
         multiple-cursors
         paredit
         puppet-mode
         rhtml-mode
         scss-mode
         smex
         undo-tree
         web-mode
         xterm-frobs
         yasnippet))

(if *is-a-mac*
    (progn
      (add-to-list 'bw-packages 'exec-path-from-shell)
      (add-to-list 'bw-packages 'edit-server)))

(el-get 'sync bw-packages)

(provide 'init-el-get)
