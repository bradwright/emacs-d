;;; load path stuff

;; built-in modes
(defconst modes-dotfiles-dir
  (bw-join-dirs dotfiles-dir "modes")
  "Configuration for modes")

(bw-add-to-load-path modes-dotfiles-dir)

;; tmp directory for storing stupid crap
(make-directory (setq tmp-local-dir (bw-join-dirs dotfiles-dir ".tmp/")) t)

(provide 'init-paths)
