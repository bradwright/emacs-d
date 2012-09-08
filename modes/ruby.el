;;; -*- lexical-binding: t -*-

;; Ruby mode

(use-package ruby-mode
  :mode (("\\.rb$" . ruby-mode)
         ("[vV]agrantfile$" . ruby-mode)
         ("[gG]emfile$" . ruby-mode)
         ("[rR]akefile$" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("\\.rabl$" . ruby-mode)
         ("[cC]apfile$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode))
  :config
  (progn
    ;; Rails project setup
    (defun eproject-rails-config ()
      "Various settings for Rails projects"

      ;; We don't want to compile SCSS in Rails because the asset pipeline
      ;; does it for us
      (set (make-local-variable 'scss-compile-at-save) nil))

    (add-hook 'ruby-on-rails-project-file-visit-hook 'eproject-rails-config)

    ;; Ruby has a lot of camel case
    (subword-mode)

    ;; this variable is stupid - apparently Ruby needs its own indent
    (setq ruby-indent-level 2))

  ;; fix syntax highlighting for Cucumber Step Definition regexps
  (add-to-list 'ruby-font-lock-syntactic-keywords
               '("\\(\\(\\)\\(\\)\\|Given\\|When\\|Then\\)\\s *\\(/\\)[^/\n\\\\]*\\(\\\\.[^/\n\\\\]*\\)*\\(/\\)"
                 (4 (7 . ?/))
                 (6 (7 . ?/)))))

(add-to-list 'auto-mode-alist '("\\.css\\.erb$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\.erb$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.json\\.erb$" . json-mode))

;; add a bundle version of IRB shell
(use-package rinari
  :config
  (progn
    (add-to-list 'inf-ruby-implementations '("bundle-ruby" . "bundle exec irb --inf-ruby-mode -r irb/completion"))))
