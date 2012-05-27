;; Color themes and graphical embellishment

(defvar bw-after-frame-raise-hook '()
  "Hook called after a frame is raised. Functions receive a
  single arg, which is the frame that was raised.")

;; Theme files
(setq
 theme-dotfiles-dir
 (concat dotfiles-dir "vendor/themes"))

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


;; TODO: what does this do?
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

(when (display-graphic-p)
  ;; show help in the echo area instead of as a tooltip
  (tooltip-mode -1)
  ;; make fringe-mode 4 pixels
  (fringe-mode 4))

(when (fboundp 'prog-mode)
  (add-hook 'prog-mode-hook 'local-hl-line-mode-on))
;; Don't blink the cursor
(blink-cursor-mode -1)

;; stop beeping at me
(setq visible-bell t)

;; if we do use line numbers, format them
(setq linum-format " %d ")
