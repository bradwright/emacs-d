;;; load path stuff

;; external libraries I might have collected via submodules etc.
(defconst vendor-dotfiles-dir
  (bw-join-dirs dotfiles-dir "vendor")
  "Vendorised Emacs libraries")

(bw-add-to-load-path vendor-dotfiles-dir)

(provide 'init-paths)
