;; eproject mode configuration

(use-package eproject
  :diminish eproject-mode
  :config
  (progn
    ;; ruby on rails special type
    (define-project-type ruby-on-rails-git (generic-git)
      (and (look-for ".git")
           (look-for "Gemfile")
           (look-for "config/application.rb"))
      :main-file "Gemfile")

    (defun rails-eproject-test (&optional only-this-file)
      "Runs local tests"
      (interactive "P")
      (if (eq only-this-file nil)
          (compile "bundle exec rake" t)
        (compile (concat "bundle exec ruby -I test " (buffer-file-name)) t)))

    (defun rails-eproject-hook ()
      "Set up some local variables"
      (put 'scss-sass-command 'safe-local-variable 'stringp)

      ;; (shell-command "git ls-files | /usr/local/bin/ctags -e -f - -L - 1> TAGS 2> /dev/null")

      (set (make-local-variable 'inf-ruby-default-implementation) "bundle-ruby"))

    (add-hook 'ruby-on-rails-git-project-file-visit-hook 'rails-eproject-hook)

    (defun bw-eproject-find-files ()
      "If we're in a Git project, use git ls-files to look up the
files, because it won't try to open any .gitignored files."
      (interactive)
      (if (member (eproject-type) '(generic-git ruby-on-rails-git))
          (let* ((default-directory (eproject-root))
                 (files-alist
                  (split-string
                   (shell-command-to-string "git ls-files --exclude-standard -co"))))
            (find-file
             (concat
              (eproject-root)
              (ido-completing-read
               (format "Find file: %s" (bw-shorten-dir (eproject-root)))
               files-alist))))
        (eproject-find-file)))

    (setq eproject-completing-read-function 'eproject--ido-completing-read)

    (use-package eproject-extras
      :init
      :bind ("C-c f" . bw-eproject-find-files))))
