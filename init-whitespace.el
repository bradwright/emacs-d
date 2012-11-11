(use-package whitespace
  :init
  :diminish whitespace-mode
  (progn
    ;; display only tails of lines longer than 80 columns, tabs and
    ;; trailing whitespaces
    ;; style information is here: http://www.emacswiki.org/emacs/WhiteSpace
    (setq whitespace-line-column 80
          whitespace-style '(face tabs trailing lines-tail))

    ;; turn on whitespace mode globally
    (global-whitespace-mode t)

    ;; this causes issues with ERC colour alias
    (setq whitespace-global-modes '(not erc-mode))))

(provide 'init-whitespace)
