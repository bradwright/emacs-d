;;; global interface changes

;; always highlight syntax
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Highlight matching parentheses
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; show keystrokes
(setq echo-keystrokes 0.1)

;; Always show line number in the mode line
(line-number-mode 1)
;; ... and show the column number
(column-number-mode 1)

;; Show bell
(setq visible-bell t)

;; auto-pad linum-mode according to length of buffer
(setq linum-format (lambda (line)
  (propertize
   (format (concat " %"
                   (number-to-string
                    (length (number-to-string
                             (line-number-at-pos (point-max)))))
                   "d ")
           line)
   'face 'linum)))

(provide 'init-interface)
