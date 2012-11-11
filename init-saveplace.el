;; Saveplace
;;   - places cursor in the last place you edited file
(use-package saveplace
  :config
  (progn
    (setq-default save-place t)
    ;; Keep places in the load path
    (setq save-place-file (file-name-as-directory (concat tmp-local-dir "emacs-places")))))

(provide 'init-saveplace)
