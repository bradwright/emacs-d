;; Color themes and graphical embellishment

;; Theme files
(setq
 theme-dotfiles-dir
 (concat dotfiles-dir "vendor/themes"))
(add-to-list 'load-path theme-dotfiles-dir)

(let ((base theme-dotfiles-dir))
  (add-to-list 'custom-theme-load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'custom-theme-load-path name)))))

;; make sure all themes are loaded
;; no themes right now

(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(tooltip-mode -1)
(blink-cursor-mode -1)

;; stop beeping at me
(setq visible-bell t)

;; if we do use line numbers, format them
(setq linum-format " %d ")
