(use-package scss-mode
  :config
  (progn
    (put 'scss-sass-command 'safe-local-variable 'stringp)
    (put 'css-indent-offset 'safe-local-variable 'integerp)
    (put 'scss-compile-at-save 'safe-local-variable 'booleanp)))

(provide 'init-scss)
