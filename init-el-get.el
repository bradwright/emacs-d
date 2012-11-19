(defconst el-get-base-dir
  (bw-join-dirs dotfiles-dir "el-get"))

(bw-add-to-load-path el-get-base-dir)
(make-directory el-get-base-dir t)

(defconst bw-el-get-packages
  '((:name magit)
    (:name multiple-cursors))
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
  (let ((my-packages (mapcar 'el-get-source-name bw-el-get-packages)))
    (el-get-cleanup my-packages)
    (el-get 'sync my-packages)))

(use-package el-get
  :init (bw-sync-packages))

(provide 'init-el-get)
