;; Mac hostnames have .local or similar appended
(setq system-name (car (split-string system-name "\\.")))

;; OS X ls doesn't support --dired
(setq dired-use-ls-dired nil)

;; Even though we may have set the Mac OS X Terminal's Alt key as the
;; emacs Meta key, we want to be able to insert a '#' using Alt-3 in
;; emacs as we would in other programs.
(fset 'insert-pound "#")
(define-key global-map "\M-3" 'insert-pound)

;; run Solarized Dark on OS X
(add-hook 'after-init-hook (lambda ()
                             (load-theme 'solarized-dark t)))

(when (or *is-carbon-emacs*
	      *is-cocoa-emacs*)
  (progn
    ;; Mac GUI stuff
    ;; set my favourite Mac font as the default font
    (set-face-attribute 'default nil :foundry "apple"
                        :family "Inconsolata" :height 160)
    ;; (set-face-attribute 'default nil :foundry "adobe"
    ;;                     :family "Source Code Pro" :height 150)

    ;; meta key configuration

    ;; This makes left-option do M-
    (setq ns-alternate-modifier 'meta)
    ;; ... and right-option just do option.
    (setq ns-right-alternate-modifier nil)

    ;; command is super
    (setq ns-command-modifier 'super)
    ;; fn does nothing special for Emacs
    (setq ns-function-modifier 'nil)))

(unless (display-graphic-p)
  (progn
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
    (setq interprogram-paste-function 'copy-from-osx)))

(provide 'init-osx)
