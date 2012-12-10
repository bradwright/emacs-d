;; el-get configuration

;; my own package definitions
(defconst el-get-sources
  '((:name ack-and-a-half :type elpa)
    (:name color-theme-solarized
           :type github
           :pkgname "sellout/emacs-color-theme-solarized"
           :prepare (progn
                  (add-to-list 'custom-theme-load-path default-directory)
                  (autoload 'color-theme-solarized-light "color-theme-solarized"
                    "color-theme: solarized-light" t)
                  (autoload 'color-theme-solarized-dark "color-theme-solarized"
                    "color-theme: solarized-dark" t)))
    (:name diminish :type elpa)
    (:name flymake-cursor :type elpa)
    (:name idomenu :type elpa)
    (:name iedit :type elpa)
    ;; js2-mode changed to be better in Emacs24
    (:name js2-mode
       :type github
       :branch "emacs24"
       :pkgname "mooz/js2-mode"
       :prepare (autoload 'js2-mode "js2-mode" nil t))
    ;; this replaces the built-in package.rcp
    ;; because it clobbers the package-archives
    (:name package
           :builtin 24
           :features package
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
             (defconst package-base-dir
               (bw-join-dirs dotfiles-dir "elpa"))
             (defconst package-install-dir
               (bw-join-dirs package-base-dir "installed"))

             (make-directory package-base-dir t)
             (make-directory package-install-dir t)

             (bw-add-to-load-path package-base-dir)

             ;; this is to set up packages
             (setq package-user-dir package-install-dir)))
    (:name paredit :type elpa))
    "Packages I've modified the recipes for.")

(defun bw-el-get-cleanup (packages)
  "Remove installed packages not explicitly declared"
  (let* ((packages-to-keep
          (el-get-dependencies (mapcar 'el-get-as-symbol packages)))
         (packages-to-remove
          (set-difference (mapcar 'el-get-as-symbol
                                  (el-get-list-package-names-with-status
                                   "installed")) packages-to-keep)))
    (mapc 'el-get-remove packages-to-remove)))

;; init-* files are stored here
(defconst el-get-base-dir
  (bw-join-dirs dotfiles-dir "el-get"))

(bw-add-to-load-path el-get-base-dir)
(make-directory el-get-base-dir t)
(setq el-get-dir el-get-base-dir)

(require 'el-get)

(eval-after-load 'el-get
  (progn

    ;; ... but we also want to store installed packages there
    (defun bw-sync-packages ()
      "Syncs and cleans up packages"
      (interactive)
      (let ((my-packages bw-el-get-packages)
            (el-get-install-skip-emacswiki-recipes))
        (bw-el-get-cleanup my-packages)
        (el-get 'sync my-packages)))

    (defconst bw-el-get-packages
      (append '(browse-kill-ring
                eproject
                expand-region
                git-modes
                magit
                magithub
                multiple-cursors
                rhtml-mode
                smex
                undo-tree
                use-package
                web-mode
                yasnippet)
              (mapcar 'el-get-source-name el-get-sources)))
    (bw-sync-packages)))

(provide 'init-el-get)
