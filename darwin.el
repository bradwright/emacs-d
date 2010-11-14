;; OS X Specific configuration

;; When we have a window system, do some specific stuff
(when window-system
  (progn
    ;; set my favourite Mac font as the default font
    (set-face-font 'default "-apple-inconsolata-medium-r-normal--15-150-72-72-m-150-iso10646-1"))
  ;; Free up the option key for special characters
  (setq ns-alternate-modifier 'none)
  (setq ns-command-modifier 'meta)
  (setq ns-function-modifier 'super)
  ;; Make Emacs behave like a Mac app
  ;; Gotten from: http://github.com/samsonjs/config/blob/master/emacs
  (global-set-key "\M-o" 'find-file)
  (global-set-key "\M-s" 'save-buffer)
  (global-set-key "\M-z" 'undo)
  (global-set-key [(meta down)] 'end-of-buffer)
  (global-set-key [(meta up)] 'beginning-of-buffer)
  (global-set-key [(meta right)] 'end-of-line)
  (global-set-key [(meta left)] 'beginning-of-line)
  ;; can't seem to un-hijack cmd-`, so make it do something useful
  (global-set-key "\M-`" 'other-window-in-any-frame))

;;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; do the Chrome thing
(require 'edit-server)
(edit-server-start)

;; Edit path crap
(push "/Users/bradleyw/bin" exec-path)
(push "/Users/bradleyw/Projects/homebrew/bin" exec-path)

;; switch to the next window, in any visible frame
(defun other-window-in-any-frame (&optional arg)
  "Switch to the next window using `next-window', with ALL-FRAMES
set to 'visible.  If the next window is on a different frame
switch to that frame first using `select-frame-set-input-focus'.

If N is non-nil switch to the nth next window."
  (interactive "p")
  (while (> arg 0)
    (let ((window (next-window (selected-window) nil 'visible)))
      (when (not (member window (window-list)))
        (dolist (frame (delq (selected-frame) (frame-list)))
          (when (member window (window-list frame))
            (select-frame-set-input-focus frame))))
      (select-window window))
    (decf arg)))
