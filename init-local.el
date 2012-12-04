;; local overrides

(setq local-dotfiles-dir (bw-join-dirs dotfiles-dir "local"))

(setq
 bw-user-config (concat local-dotfiles-dir user-login-name ".el")
 bw-system-config (concat local-dotfiles-dir system-name ".el"))

(defun load-local-configs ()
  (when (file-exists-p bw-user-config)
    (load bw-user-config))
  (when (file-exists-p bw-system-config)
    (load bw-system-config)))

(add-hook 'after-init-hook 'load-local-configs)

(provide 'init-local)
