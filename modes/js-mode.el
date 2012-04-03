;; special configuration for JS-mode

(setq inferior-js-program-command "node")
(add-hook
 'inferior-js-mode-hook
 (lambda ()
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
        output))))))


;; Flymake uses node.js jslint
(defun flymake-jslint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "jslint" (list "--output" "simple"
                         "--exit0"
                         local-file))))

(when (load "flymake" t)
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.js\\'" flymake-jslint-init))
  (add-to-list 'flymake-err-line-patterns
               '("jslint:\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\(.*\\)$"
                 nil 1 2 3)))

(defun pick-javascript-mode ()
  "Picks Javascript mode based on buffer length, because js2-mode
can crash on large files"
  (interactive)
  (if (> (get-buffer-line-length) 1000)
      (change-mode-if-not-in-mode 'js-mode)
    (change-mode-if-not-in-mode 'js2-mode)))

(add-hook 'js-mode-hook 'turn-on-flymake-mode)
(add-hook 'js2-mode-hook 'turn-on-flymake-mode)

;; flip between modes
(add-hook 'js-mode-hook 'pick-javascript-mode)
(add-hook 'js2-mode-hook 'pick-javascript-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

;; Don't highlight missing variables in js2-mode: we have jslint for
;; that
(setq js2-highlight-external-variables nil)
