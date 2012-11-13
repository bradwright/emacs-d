;; customise solarized-dark/light themes
(defadvice load-theme
  (before load-theme)
  (let ((theme-name (ad-get-arg 0)))
    (when (or (eq theme-name 'solarized-dark)
              (eq theme-name 'solarized-light))
      (progn
        (custom-set-faces
         '(magit-diff-add ((t (:inherit diff-added :weight normal))))
         '(magit-diff-del ((t (:inherit diff-removed :weight normal))))
         '(diff-refine-change ((t (:inherit diff-refine-change :background nil))))
         '(iedit-occurrence ((t (:inherit lazy-highlight))))
         '(match ((t (:inherit lazy-highlight :reverse t))))
         '(erb-face ((t (:background nil))))
         '(erb-out-delim-face ((t (:inherit erb-exec-delim-face :foreground "#b58900")))))
        (when (and (display-graphic-p) *is-a-mac*)
          ;; My Macs have the --srgb flag set
          (setq solarized-broken-srgb nil))))))

(ad-activate 'load-theme)

(defun bw-toggle-solarized ()
  "Toggles between solarized light and dark"
  (interactive)
  (cond
   ((custom-theme-enabled-p 'solarized-dark)
    (progn
      (disable-theme 'solarized-dark)
      (enable-theme 'solarized-light)))
   ((custom-theme-enabled-p 'solarized-light)
    (progn
      (disable-theme 'solarized-light)
      (enable-theme 'solarized-dark)))))

(provide 'init-solarized)
