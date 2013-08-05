;; Mac hostnames have .local or similar appended
(setq system-name (car (split-string system-name "\\.")))

;; OS X ls doesn't support --dired
(setq dired-use-ls-dired nil)

;; Even though we may have set the Mac OS X Terminal's Alt key as the
;; emacs Meta key, we want to be able to insert a '#' using Alt-3 in
;; emacs as we would in other programs.
(fset 'insert-pound "#")
(define-key global-map "\M-3" 'insert-pound)

(when (or *is-carbon-emacs*
	      *is-cocoa-emacs*)
  ;; Use solarized-dark from solarized-theme
  (load-theme 'solarized-dark t)

  ;; Mac GUI stuff
  ;; set my favourite Mac font as the default font
  (custom-set-faces
   '(default ((t (:height 180 :family "Inconsolata")))))

  ;; meta key configuration

  ;; This makes left-option do M-
  (setq ns-alternate-modifier 'meta)
  ;; ... and right-option just do option so I can still type
  ;; alternate characters.
  (setq ns-right-alternate-modifier nil)

  ;; command is super
  (setq ns-command-modifier 'super)

  ;; set fn to hyper
  (setq ns-function-modifier 'hyper)

  (global-set-key (kbd "s-<return>") 'ns-toggle-fullscreen)

  (global-set-key (kbd "s-p") 'bw-find-file-git-ls-files-completing)
  (global-set-key (kbd "s-F") 'bw-git-grep)
  (global-set-key (kbd "s-b") 'bw-eproject-ido-switch-buffers)
  (global-set-key (kbd "H-g") 'magit-status)

  ;; we pretty much never ever want to background emacs
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))

  ;; Don't open files from the workspace in a new frame
  (setq ns-pop-up-frames nil)

  ;; Emacs launched from the desktop doesn't inherit the shell
  ;; env. This package:
  ;; https://github.com/purcell/exec-path-from-shell automatically
  ;; mirrors the PATH and other environment variables from a login
  ;; shell, ensuring that things work correctly.
  (use-package exec-path-from-shell
    :init
    (progn
      ;; copy SHELL correctly
      (setq exec-path-from-shell-variables '("PATH" "MANPATH" "SHELL"))
      ;; copy shell PATH across to exec-path
      (exec-path-from-shell-initialize)))

  ;; load some OSX things
  (require 'init-edit-server))

(unless (display-graphic-p)
  ;; Configuration to make Emacs run semi-normally in an OS X
  ;; terminal

  ;; XXX: strongly recommended to run in iTerm2, as it's more
  ;; configurable than Terminal.app.

  ;; Make sure cut/paste works properly. Gotten from:
  ;; http://mindlev.wordpress.com/2011/06/13/emacs-in-a-terminal-on-osx/#comment-20
  (defun copy-from-osx ()
    "Copies the current clipboard content using the `pbcopy` command"
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    "Copies the top of the kill ring stack to the OSX clipboard"
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  ;; Override defaults to use the mac copy and paste
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

(provide 'init-osx)
