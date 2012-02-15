(progn
  ;; set my favourite Mac font as the default font
  ;; -apple-Andale_Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1
  (set-face-font 'default "-apple-inconsolata-medium-r-normal--15-150-72-72-m-150-iso10646-1"))
  ;; Free up the option key for special characters
(setq ns-alternate-modifier 'none)
(setq ns-command-modifier 'meta)
(setq ns-function-modifier 'super)
;; can't seem to un-hijack cmd-`, so make it do something useful
(global-set-key "\M-`" 'other-window-in-any-frame)
;; Set cmd-H to hide Emacs and cmd-shift-h to hide others, as usual in Mac OS
;; X. Usually bound to mark-paragraph
(global-set-key "\M-h" 'ns-do-hide-emacs)
(global-set-key "\M-H" 'ns-do-hide-others)
;; In Emacs 23 (Cocoa) in Snow Leopard, Apple delete key deletes backward, not
;; forward as is usual. This fixes this behaviour.
(normal-erase-is-backspace-mode 1)

;; switch to the next window, in any visible frame
(defun other-window-in-any-frame (&optional arg)
  "Switch to the next window using `next-window', with ALL-FRAMES
set to 'visible.  If the next window is on a different frame
switch to that frame first using `select-frame-set-input-focus'.

If N is non-nil switch to the nth next window."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((gt-or-lt (if (> arg 0) #'> #'<))
        (sign (if (> arg 0) 1 -1)))
    (while (apply gt-or-lt arg '(0))
      (let ((window (if (= sign 1)
                        (next-window (selected-window) nil 'visible)
                      (previous-window (selected-window) nil 'visible))))
        (when (not (member window (window-list)))
      (dolist (frame (delq (selected-frame) (frame-list)))
        (when (member window (window-list frame))
          (select-frame-set-input-focus frame))))
        (select-window window))
      (setq arg (- arg sign)))))

;; Open any new buffers in the existing frame
(setq ns-pop-up-frames nil)


;; Use Solarized-dark on OS X
;; but load it after custom has loaded, so it's marked safe
(add-hook 'bw-after-custom-load-hook
          (lambda ()
            (load-theme 'solarized-dark t)
            ;; we can turn hl-line-mode on...
            (global-hl-line-mode 1)
            ;; ... because we clobber the region style
            (set-face-attribute 'region nil :background "#d33682" :foreground "#fdf6e3")))
