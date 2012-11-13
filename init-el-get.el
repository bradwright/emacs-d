(defconst el-get-base-dir
  (bw-join-dirs dotfiles-dir "el-get"))

(bw-add-to-load-path el-get-base-dir)
(make-directory el-get-base-dir t)

(defconst bw-el-get-packages
  '(magit)
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
  (el-get-cleanup bw-el-get-packages)
  (el-get 'sync bw-el-get-packages))

(use-package el-get
  :commands (el-get
             el-get-install
             el-get-update
             el-get-list-packages))

(provide 'init-el-get)
