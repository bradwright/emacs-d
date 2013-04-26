;;; general editing configuration

;; Turn off mouse interface early in startup to avoid momentary flash
;; of things I don't want.
(progn
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

;; Don't show the splash screen
(setq inhibit-startup-screen t
      ;; Show the *scratch* on startup
      initial-buffer-choice t)

;; I got sick of typing "yes"
(defalias 'yes-or-no-p 'y-or-n-p)

;; I prefer spaces over tabs
(setq-default
 indent-tabs-mode nil
 ;; ... and I prefer 4-space indents
 tab-width 4)

;; UTF-8 please!
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Don't clobber things in the system clipboard when killing
(setq save-interprogram-paste-before-kill t)

;; nuke trailing whitespace when writing to a file
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; http://emacs-fu.blogspot.hk/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position
            2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
            (line-beginning-position 2)))))

;; enable hl-line-mode for prog-mode
(add-hook 'prog-mode-hook 'bw-enable-hl-line-mode)

;; turn on electric-indent-mode
(add-hook 'prog-mode-hook 'bw/turn-on-electric-indent-mode)

;; I want to use narrowing
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; I want to use scrolling
(put 'scroll-left 'disabled nil)

;; Make rectangle selection better
(cua-selection-mode 1)

(provide 'init-editing)
