;;; load path stuff

;; external libraries I might have collected via submodules etc.
(defconst vendor-dotfiles-dir
  (bw-join-dirs dotfiles-dir "vendor")
  "Vendorised Emacs libraries")

(bw-add-to-load-path vendor-dotfiles-dir)

;; automatically add everything under vendor to load-path
(dolist (f (directory-files vendor-dotfiles-dir))
  (let ((name (concat vendor-dotfiles-dir "/" f)))
    (when (and (file-directory-p name)
               (not (equal f ".."))
               (not (equal f ".")))
      (bw-add-to-load-path name))))

(provide 'init-paths)
