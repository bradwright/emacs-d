;; force ansi-term to be utf-8 after it launches
(defadvice ansi-term
  (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

(provide 'init-ansi-term)
