(setq
 tmp-backups-dir (bw-join-dirs tmp-local-dir "backups")
 tmp-autosaves-dir (bw-join-dirs tmp-local-dir "autosaves"))

(make-directory tmp-backups-dir t)
(make-directory tmp-autosaves-dir t)

(setq
 backup-by-copying t  ; Don't clobber symlinks
 backup-directory-alist `((".*" . ,tmp-backups-dir))
 auto-save-file-name-transforms `((".*" ,tmp-autosaves-dir t))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)   ; Use versioned backups

(provide 'init-backups)
