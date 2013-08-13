;; eproject-mode is a minimal "project" wrapper
;; https://github.com/jrockway/eproject

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

      ;; We don't want to compile SCSS in Rails because the asset pipeline
      ;; does it for us
      (set (make-local-variable 'scss-compile-at-save) nil)
      (set (make-local-variable 'css-indent-offset) 2)
      (set (make-local-variable 'inf-ruby-default-implementation) "bundle-ruby"))

    (add-hook 'ruby-on-rails-git-project-file-visit-hook 'rails-eproject-hook)

    (defun bw-eproject-find-files ()
      "If we're in a Git project, use git ls-files to look up the
files, because it won't try to open any .gitignored files."
      (interactive)
      (if (member (eproject-type) '(generic-git))
          (magit-find-file-completing-read)
        (eproject-find-file)))

    (setq eproject-completing-read-function 'eproject--ido-completing-read)

    (use-package eproject-extras
      :init
      (progn
        (define-key (current-global-map) [remap eproject-switch-to-buffer] 'bw-eproject-ido-switch-buffers)
        (define-key (current-global-map) [remap eproject-find-file] 'bw-eproject-find-files)))))

(provide 'init-eproject)
