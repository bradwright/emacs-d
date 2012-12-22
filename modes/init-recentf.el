(recentf-mode 1)

(setq recentf-auto-cleanup 'never
      recentf-exclude '("[/\\]\\.elpa/" "[/\\]\\.ido\\.last\\'" "[/\\]\\.git/" "[/\\]\\.gz\\'")
      recentf-save-file (expand-file-name ".recentf" tmp-local-dir))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Recently: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

(provide 'init-recentf)
