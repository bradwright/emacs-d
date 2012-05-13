;; OS X Specific configuration

;;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; copy shell PATH across to exec-path

(defun set-darwin-path ()
  "Get PATH from the shell, as the OSX environment is broken and weird"
  (let ((darwin-path (env-var-from-login-shell "PATH")))
    (setq exec-path (split-string darwin-path path-separator))
    (setenv "PATH" darwin-path)))
(set-darwin-path)

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
  (load-custom-themes)
  (load-theme 'solarized-dark t))

(when (or
       (display-graphic-p)
       (search "Solarized" (getenv "ITERM_PROFILE")))
  (add-hook 'bw-after-custom-load-hook 'load-solarized-dark-theme))

;; Even though we may have set the Mac OS X Terminal's Alt key as the
;; emacs Meta key ...
;; ... we want to be able to insert a '#' using
;; Alt-3 in emacs as we would in other programs
(fset 'insert-pound "#")
(define-key global-map "\M-3" 'insert-pound)
