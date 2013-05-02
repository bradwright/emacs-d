;; customise solarized-dark/light themes
(defun bw/toggle-theme (theme-1 theme-2)
  "Turn off the current theme and turn on a new one"
  (cond
   ((custom-theme-enabled-p theme-1)
    (--bw-switch-theme theme-2 theme-1))
   ((custom-theme-enabled-p theme-2)
    (--bw-switch-theme theme-1 theme-2))))

(defun --bw-switch-theme (theme-to-enable theme-to-disable)
  (disable-theme theme-to-disable)
  (when (custom-theme-enabled-p theme-to-enable)
    (enable-theme theme-to-enable))
  (load-theme theme-to-enable t))

(defun bw-toggle-solarized ()
  "Toggles between solarized light and dark"
  (interactive)
  (bw/toggle-theme 'solarized-dark 'solarized-light))

;; We load the theme after init because we might have changed some
;; variables in customize.
(defun bw-load-solarized ()
  "Loads Solarized light and dark"
  (interactive)
  (custom-set-faces
   '(erm-syn-warnline ((t (:box nil))))
   '(erm-syn-errline  ((t (:box nil))))
   '(enh-ruby-string-delimiter-face ((t (:foreground "#dc322f" :background nil))))
   '(enh-ruby-regexp-delimiter-face ((t (:foreground "#dc322f" :background nil))))
   '(enh-ruby-op-face ((t (:inherit default))))
   '(magit-diff-add ((t (:inherit diff-added :weight normal))))
   '(magit-diff-del ((t (:inherit diff-removed :weight normal))))
   '(diff-refine-change ((t (:inherit diff-refine-change :background nil :weight normal))))
   '(iedit-occurrence ((t (:inherit lazy-highlight))))
   '(match ((t (:inherit lazy-highlight :reverse t))))
   '(erb-face ((t (:background nil))))
   '(erb-out-delim-face ((t (:inherit erb-exec-delim-face :foreground "#b58900")))))
  (when (and (display-graphic-p) *is-a-mac*)
    ;; My Macs have the --srgb flag set
    ;; so we need to check if it was compiled here.
    (when (string-match (concat "on " (system-name)) (emacs-version))
      (custom-set-variables
       '(solarized-broken-srgb nil))))
  (load-theme 'solarized t)

  (add-hook 'after-make-frame-functions
            (lambda (frame)
              "Reenable solarized"
              (enable-theme 'solarized)))

  (setq-default frame-background-mode 'dark)
  (set-frame-parameter nil 'background-mode 'dark)
  (enable-theme 'solarized)

  (custom-set-faces
   `(term-color-black ((t (:inherit term-color-black :background ,(face-attribute 'term-color-black :foreground)))))
   `(term-color-red ((t (:inherit term-color-red :background ,(face-attribute 'term-color-red :foreground)))))
   `(term-color-green ((t (:inherit term-color-green :background ,(face-attribute 'term-color-green :foreground)))))
   `(term-color-yellow ((t (:inherit term-color-yellow :background ,(face-attribute 'term-color-yellow :foreground)))))
   `(term-color-blue ((t (:inherit term-color-blue :background ,(face-attribute 'term-color-blue :foreground)))))
   `(term-color-magenta ((t (:inherit term-color-magenta :background ,(face-attribute 'term-color-magenta :foreground)))))
   `(term-color-cyan ((t (:inherit term-color-cyan :background ,(face-attribute 'term-color-cyan :foreground)))))
   `(term-color-white ((t (:inherit term-color-white :background ,(face-attribute 'term-color-white :foreground)))))))

(provide 'init-solarized)
