;; el-get configuration

;; init-* files are stored here
(defconst el-get-base-dir
  (bw-join-dirs dotfiles-dir "el-get"))

(bw-add-to-load-path el-get-base-dir)
(make-directory el-get-base-dir t)

;; ... but we also want to store installed packages there
(setq el-get-dir el-get-base-dir)

;; my own packages
(defconst el-get-sources
  '((:name ack-and-a-half :type elpa)
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
             ;; override default packaging list
             (setq package-archives
                   '(("gnu" . "http://elpa.gnu.org/packages/")
                     ("marmalade" . "http://marmalade-repo.org/packages/")
                     ("melpa" . "http://melpa.milkbox.net/packages/"))))))
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

(defun bw-sync-packages ()
  "Syncs and cleans up packages"
  (interactive)
  (let ((my-packages bw-el-get-packages)
        (el-get-install-skip-emacswiki-recipes))
    (bw-el-get-cleanup my-packages)
    (el-get 'sync my-packages)))

(use-package el-get
  :init
  (progn
    (defconst bw-el-get-packages
      (append '(expand-region
                git-modes
                magit
                multiple-cursors)
              (mapcar 'el-get-source-name el-get-sources)))
    (bw-sync-packages)))

(provide 'init-el-get)
