(ido-mode t)
(ido-everywhere t)

(setq
 ;; Match arbitrary points in strings
 ido-enable-prefix nil
 ;; Match across entire string
 ido-enable-flex-matching t
 ;; Create a new buffer if there's no match candidate
 ido-create-new-buffer 'always
 ;; Don't try and guess if the string under point is a file
 ido-use-filename-at-point nil
 ;; case-insensitive matching
 ido-case-fold t)

(provide 'init-ido)
