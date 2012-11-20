(defconst el-get-base-dir
  (bw-join-dirs dotfiles-dir "el-get"))

(bw-add-to-load-path el-get-base-dir)
(make-directory el-get-base-dir t)
(setq el-get-dir el-get-base-dir)

(defconst el-get-sources
  '((:name ack-and-a-half :type elpa))
  "Canonical list of packages.")



(defun el-get-cleanup (packages)
  "Remove packages not explicitly declared"
  (let* ((packages-to-keep (el-get-dependencies (mapcar 'el-get-as-symbol packages)))
         (packages-to-remove (set-difference (mapcar 'el-get-as-symbol
                                                     (el-get-list-package-names-with-status
                                                      "installed")) packages-to-keep)))
    (mapc 'el-get-remove packages-to-remove)))

(defun bw-sync-packages ()
  "Syncs and cleans up packages"
  (interactive)
  (let ((my-packages bw-el-get-packages)
        (el-get-install-skip-emacswiki-recipes))
    (el-get-cleanup my-packages)
    (el-get 'sync my-packages)))

(use-package el-get
  :init
  (progn
    ;; override default packaging list
    (setq package-archives
          '(("gnu" . "http://elpa.gnu.org/packages/")
            ("marmalade" . "http://marmalade-repo.org/packages/")
            ("melpa" . "http://melpa.milkbox.net/packages/")))
    (defconst bw-el-get-packages
      (append '(expand-region
                magit
                multiple-cursors)
              (mapcar 'el-get-source-name el-get-sources)))
    (bw-sync-packages)))

(provide 'init-el-get)
