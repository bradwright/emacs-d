;; eshell
(setq eshell-directory-name (bw-join-dirs tmp-local-dir "eshell"))

(eval-after-load 'esh-opt
  '(progn
     ;; we need this to override visual commands
     (require 'em-term)
     ;; If I try to SSH from an eshell, launch it in ansi-term instead
     (add-to-list 'eshell-visual-commands "ssh")
     ;; vagrant ssh is similar
     (add-to-list 'eshell-visual-subcommands '("vagrant" "ssh"))))

;; fix ANSI colour issues from test runners etc.
(add-hook 'eshell-preoutput-filter-functions
          'ansi-color-filter-apply)

;; launch an eshell from keys
(global-set-key (kbd "C-c e") 'eshell)

;; Eshell aliases
(defun eshell/git-log ()
  (magit-log))

(defun eshell/e (file)
  (find-file file))

(provide 'init-eshell)
