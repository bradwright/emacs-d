;; Theme files
(setq theme-dotfiles-dir (file-name-as-directory (concat dotfiles-dir "vendor/themes/")))
(make-directory theme-dotfiles-dir t)

;; load all custom themes
(defun load-custom-themes ()
  "Adds custom themes directory to themefiles"
  (interactive)
  (let ((base theme-dotfiles-dir))
    (add-to-list 'custom-theme-load-path base)
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (add-to-list 'custom-theme-load-path name))))))

(load-custom-themes)

(provide 'init-themes)
