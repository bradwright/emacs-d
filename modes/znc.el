;; erc configuration for IRC

(require 'erc)

(require 'erc-services nil t)
(erc-services-mode 1)

;; don't show join/part etc.
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;; switch to ERC with Ctrl+c Ctrl+e
(global-set-key (kbd "C-c C-e") 'znc-all) ;; ERC

(setq znc-servers
      (quote
       (("brewdog.bradleywright.net" 60667 nil
         ((freenode\.net "intranation" (get-keychain-password "znc-intranation"))
          (redsrc "brad" (get-keychain-password "znc-brad")))))))
