;;; -*- lexical-binding: t -*-

;; JS2 mode is a Javascript major mode:
;; https://github.com/mooz/js2-mode

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (progn
    (rename-modeline "js2-mode" js2-mode "JS2")

    ;; Use Node.js REPL for JS shells
    (setq inferior-js-program-command "node")

    ;; modify js-related modes at runtime
    (defun bw-js-mode-hook ()
      "Setup for JS inferior mode"
      ;; We like nice colors
      (ansi-color-for-comint-mode-on)
      ;; Deal with some prompt nonsense
      (add-to-list
       'comint-preoutput-filter-functions
       (lambda (output)
         (replace-regexp-in-string
          ".*1G\.\.\..*5G" "..."
          (replace-regexp-in-string
           ".*1G.*3G" "js> "
           output)))))

    (add-hook 'inferior-js-mode-hook 'bw-js-mode-hook)

    ;; Camel case is popular in JS
    (subword-mode)

    ;; Flymake uses node.js jslint
    (defun flymake-jslint-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (expand-file-name (file-relative-name
                                            temp-file
                                            (file-name-directory buffer-file-name))))
             (node-bin (concat dotfiles-dir "node_modules/.bin"))
             (exec-path (add-to-list 'exec-path node-bin)))
        (list "jslint" (list "--terse" local-file))))

    (when (load "flymake" t)
      (add-to-list 'flymake-allowed-file-name-masks
                   '("\\.js\\'" flymake-jslint-init))
      ;; jslint lines look like:
      ;; jslint:25:45:Missing trailing ; character
      (add-to-list 'flymake-err-line-patterns
                   '("^\\(.*\\)\\(([[:digit:]]+)\\):\\(.*\\)$" 1 2 nil 3)))

    (add-hook 'js2-mode-hook 'bw-turn-on-flymake-mode)

    (setq
     ;; highlight everything
     js2-highlight-level 3
     ;; 4 space indent
     js2-basic-offset 4
     ;; idiomatic closing bracket position
     js2-consistent-level-indent-inner-bracket-p t
     ;; allow for multi-line var indenting
     js2-pretty-multiline-decl-indentation-p t
     ;; Don't highlight missing variables in js2-mode: we have jslint for
     ;; that
     js2-highlight-external-variables nil
     ;; jslint shows missing semi-colons
     js2-strict-missing-semi-warning nil)))

(provide 'init-js2)
