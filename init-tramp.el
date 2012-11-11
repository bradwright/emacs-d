(eval-after-load 'tramp
  '(progn
     ;; use SSH by default
     (setq tramp-default-method "ssh")
     ;; allow me to SSH to hosts and edit as sudo like:
     ;;   C-x C-f /sudo:example.com:/etc/something-owned-by-root
     ;; from: http://www.gnu.org/software/tramp/#Multi_002dhops
     (add-to-list 'tramp-default-proxies-alist
                  '(nil "\\`root\\'" "/ssh:%h:"))
     (add-to-list 'tramp-default-proxies-alist
                  '((regexp-quote (system-name)) nil nil))))


(provide 'init-tramp)
