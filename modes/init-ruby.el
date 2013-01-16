;; Detect some extra file types
(dolist (extension
         '("[vV]agrantfile$"
           "[gG]emfile$"
           "[rR]akefile$"
           "\\.rake$"
           "\\.rabl$"
           "[cC]apfile$"
           "\\.gemspec$"
           "\\.builder$"))
  (add-to-list 'auto-mode-alist `(,extension . ruby-mode)))

;; Ruby has a lot of camel case
(add-hook 'ruby-mode-hook 'bw-turn-on-subword-mode)

(setq
 ;; Ruby has its own indentation variable
 ruby-indent-level 2
 ;; don't deep indent parens
 ruby-deep-indent-paren nil
 ;; don't insert an encoding comment automatically
 ruby-insert-encoding-magic-comment nil
 ;; don't deep indent lists
 ruby-deep-arglist nil)

;; fix syntax highlighting for Cucumber Step Definition regexps
;; (eval-after-load 'ruby-mode
;;   '(progn
;;      (add-to-list 'ruby-font-lock-syntactic-keywords
;;                   '("\\(\\(\\)\\(\\)\\|Given\\|When\\|Then\\)\\s *\\(/\\)[^/\n\\\\]*\\(\\\\.[^/\n\\\\]*\\)*\\(/\\)"
;;                     (4 (7 . ?/))
;;                     (6 (7 . ?/))))))

(provide 'init-ruby)
