;; See:
;; https://github.com/purcell/emacs.d/blob/master/init-isearch.el

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(provide 'init-isearch)
