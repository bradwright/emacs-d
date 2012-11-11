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

(provide 'init-osx)
