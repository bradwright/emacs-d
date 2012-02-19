;; Color themes and graphical embellishment

;; Theme files
(setq
 theme-dotfiles-dir
 (concat dotfiles-dir "vendor/themes"))

;; load all custom themes
(defun load-custom-themes ()
  "Adds custom themes directory to themefiles"
  (let ((base theme-dotfiles-dir))
    (add-to-list 'custom-theme-load-path base)
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (add-to-list 'custom-theme-load-path name))))))


;; TODO: what does this do?
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; show help in the echo area instead of as a tooltip
(tooltip-mode -1)

;; Don't blink the cursor
(blink-cursor-mode -1)

;; stop beeping at me
(setq visible-bell t)

;; if we do use line numbers, format them
(setq linum-format " %d ")
