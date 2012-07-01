;;; -*- lexical-binding: t -*-

;; OS X Specific configuration

;;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")
;; # FIXME: this is to ignore Dropbox "Icon" files that seem to be
;; "Icon", but I can't figure out how to ignore that.
(add-to-list 'ido-ignore-files "Icon")

;; toggle-input-method
(setq default-input-method "MacOSX")

;; fix hostname.local stuff
(setq system-name (car (split-string system-name "\\.")))

;;; Use default Mac OS X browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; load growl.el
(add-to-list 'load-path (concat vendor-dotfiles-dir "/emacs-growl"))
(require 'growl)

;; ispell isn't available on OS X, but aspell is via Homebrew
(setq-default ispell-program-name "aspell")

;; Use Solarized-dark on OS X
;; but load it after custom has loaded, so it's marked safe
(defun load-solarized-dark-theme ()
  "Loads the solarized-dark theme"
  ;; this puts the theme on the correct path
  (load-theme 'solarized-dark t))

(add-hook 'after-init-hook 'load-solarized-dark-theme)

;; Even though we may have set the Mac OS X Terminal's Alt key as the
;; emacs Meta key, we want to be able to insert a '#' using Alt-3 in
;; emacs as we would in other programs.
(fset 'insert-pound "#")
(define-key global-map "\M-3" 'insert-pound)
