(recentf-mode 1)

(setq recentf-auto-cleanup 'never
      recentf-exclude '("[/\\]\\.elpa/" "[/\\]\\.ido\\.last\\'" "[/\\]\\.git/" "[/\\]\\.gz\\'")
      recentf-save-file (expand-file-name ".recentf" tmp-local-dir)
      ;; save 100 most recent files
      recentf-max-saved-items 100)

;; strip $HOME from the front of recentf files
(add-to-list 'recentf-filename-handlers 'abbreviate-file-name)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Recently: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

(provide 'init-recentf)
