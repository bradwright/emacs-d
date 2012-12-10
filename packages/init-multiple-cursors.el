(use-package multiple-cursors
  :bind (("C-c ." . mc/mark-next-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c ," . mc/mark-previous-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-l". mc/mark-all-like-this)))

(provide 'init-multiple-cursors)
