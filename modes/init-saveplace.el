;; Saveplace
;;   - places cursor in the last place you edited file
(use-package saveplace
  :init
  (progn
    (setq-default save-place t)
    ;; Keep places in the load path
    (setq save-place-file (expand-file-name ".emacs-places" tmp-local-dir))))

(provide 'init-saveplace)
