;; erc configuration for IRC

(eval-after-load 'erc
  '(progn
     (setq erc-prompt ">"
           erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
     (require 'erc-services nil t)
     (erc-services-mode 1)))

;; switch to ERC with Ctrl+c Ctrl+e
(global-set-key (kbd "C-c C-e") 'znc-all) ;; ERC

(setq znc-servers
      `(("brewdog.bradleywright.net" 60667 nil
        ((freenode\.net "intranation" ,(get-keychain-password "znc-intranation"))
         (redsrc "brad" ,(get-keychain-password "znc-brad"))))))
