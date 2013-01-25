(setq ido-save-directory-list-file (expand-file-name ".ido.last" tmp-local-dir))

(ido-mode t)
(ido-everywhere t)

;; Ignore shitty Dropbox icon stuff:
;; http://stackoverflow.com/a/11341239/61435
(add-to-list 'ido-ignore-files "Icon\n")

(eval-after-load 'ido
  '(setq
    ;; Speed up ido by using less candidates
    ido-max-prospects 10
    ;; Match arbitrary points in strings
    ido-enable-prefix nil
    ;; Match across entire string
    ido-enable-flex-matching t
    ;; Create a new buffer if there's no match candidate
    ido-create-new-buffer 'always
    ;; Don't try and guess if the string under point is a file
    ido-use-filename-at-point nil
    ;; case-insensitive matching
    ido-case-fold t
    ;; go back in time if required
    ido-use-virtual-buffers t))

(provide 'init-ido)
