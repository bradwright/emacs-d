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
  (progn
    (custom-set-faces
     '(magit-diff-add ((t (:inherit diff-added :weight normal))))
     '(magit-diff-del ((t (:inherit diff-removed :weight normal))))
     '(diff-refine-change ((t (:inherit diff-refine-change :background nil :weight normal))))
     '(iedit-occurrence ((t (:inherit lazy-highlight))))
     '(match ((t (:inherit lazy-highlight :reverse t))))
     '(erb-face ((t (:background nil))))
     '(erb-out-delim-face ((t (:inherit erb-exec-delim-face :foreground "#b58900")))))
    (when (and (display-graphic-p) *is-a-mac*)
      ;; My Macs have the --srgb flag set
      (custom-set-variables
       '(solarized-broken-srgb nil))))
  (load-theme 'solarized-dark t))

(provide 'init-solarized)
