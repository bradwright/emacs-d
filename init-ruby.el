;; Detect some extra file types
(dolist (extension
         '("[vV]agrantfile$"
           "[gG]emfile$"
           "[rR]akefile$"
           "\\.rake$"
           "\\.rabl$"
           "[cC]apfile$"
           "\\.gemspec$"))
  (add-to-list 'auto-mode-alist `(,extension . ruby-mode)))

(defun eproject-rails-config ()
  "Various settings for Rails projects"

  ;; We don't want to compile SCSS in Rails because the asset pipeline
  ;; does it for us
  (set (make-local-variable 'scss-compile-at-save) nil))

(add-hook 'ruby-on-rails-project-file-visit-hook 'eproject-rails-config)

;; Ruby has a lot of camel case
(add-hook 'ruby-mode-hook 'turn-on-subword-mode)

(setq
 ;; Ruby has its own indentation variable
 ruby-indent-level 2
 ;; don't deep indent parens
 ruby-deep-indent-paren nil
 ;; don't insert an encoding comment automatically
 ruby-insert-encoding-magic-comment nil)

;; fix syntax highlighting for Cucumber Step Definition regexps
(eval-after-load 'ruby-mode
  '(progn
     (add-to-list 'ruby-font-lock-syntactic-keywords
                  '("\\(\\(\\)\\(\\)\\|Given\\|When\\|Then\\)\\s *\\(/\\)[^/\n\\\\]*\\(\\\\.[^/\n\\\\]*\\)*\\(/\\)"
                    (4 (7 . ?/))
                    (6 (7 . ?/))))))

(provide 'init-ruby)
