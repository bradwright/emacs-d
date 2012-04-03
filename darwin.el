;; OS X Specific configuration

;;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; update PATH, because Darwin Emacs doesn't get PATH from bash
(set-exec-path-from-shell-PATH)

;;; Use default Mac OS X browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; load growl.el
(add-to-list 'load-path (concat vendor-dotfiles-dir "/emacs-growl"))
(require 'growl)

;; ispell isn't available on OS X, but aspell is via Homebrew
(setq-default ispell-program-name "aspell")

;; Use Solarized-dark on OS X
;; but load it after custom has loaded, so it's marked safe
(if (or
     (window-system)
     (search "Solarized" (getenv "ITERM_PROFILE")))
    (add-hook 'bw-after-custom-load-hook
              (lambda ()
                (load-custom-themes)
                (load-theme 'solarized-dark nil)
                )))

;; Even though we may have set the Mac OS X Terminal's Alt key as the
;; emacs Meta key ...
;; ... we want to be able to insert a '#' using
;; Alt-3 in emacs as we would in other programs
(fset 'insertPound "#")
(define-key global-map "\M-3" 'insertPound)
