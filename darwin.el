;; OS X Specific configuration

;;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Edit path crap
(push "/Users/bradleyw/bin" exec-path)

;; update PATH, because Darwin Emacs doesn't get PATH from bash
(set-exec-path-from-shell-PATH)

;;; Use default Mac OS X browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; move to trash when deleting files
(setq delete-by-moving-to-trash t)

;; Even though we may have set the Mac OS X Terminal's Alt key as the emacs Meta key ...
;; ... we want to be able to insert a '#' using Alt-3 in emacs as we would in other programs
(fset 'insertPound "#")
(define-key global-map "\M-3" 'insertPound)

;; load growl.el
(add-to-list 'load-path (concat vendor-dotfiles-dir "/emacs-growl"))
(require 'growl)
