;; OS X Specific configuration

;;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Edit path crap
(push "/Users/bradleyw/bin" exec-path)

;; None of this works anyway, as I don't have Erlang at home any more
(when (file-exists-p "/Users/bradleyw/Projects/homebrew/Cellar/erlang/R14B02/lib/erlang/lib/tools-2.6.6.3/emacs")
  (add-to-list 'load-path "/Users/bradleyw/Projects/homebrew/Cellar/erlang/R14B02/lib/erlang/lib/tools-2.6.6.3/emacs")
  (setq erlang-root-dir "/Users/bradleyw/Projects/homebrew/Cellar/erlang/R14B02/lib/erlang")
  (setq exec-path (cons "/Users/bradleyw/Projects/homebrew/Cellar/erlang/R14B02/lib/erlang/bin" exec-path))
  (require 'erlang-start))

;; update PATH, because Darwin Emacs doesn't get PATH from bash
(set-exec-path-from-shell-PATH)

;; Even though we may have set the Mac OS X Terminal's Alt key as the emacs Meta key ...
;; ... we want to be able to insert a '#' using Alt-3 in emacs as we would in other programs
(fset 'insertPound "#")
(define-key global-map "\M-3" 'insertPound)

;; load growl.el
(add-to-list 'load-path (concat vendor-dotfiles-dir "/emacs-growl"))
(require 'growl)
