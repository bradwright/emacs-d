;; erc configuration for IRC

(require 'erc)

(require 'erc-services nil t)
(erc-services-mode 1)

;; don't show join/part etc.
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;; from: http://emacs-fu.blogspot.com/2009/06/erc-emacs-irc-client.html
(defun djcb-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "brewdog.bradleywright.net:60667") ;; ERC already active?

    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      ;; I use ZNC so I need to hit my server
      (erc :server "brewdog.bradleywright.net" :port 60667 :nick "intranation" :password (format "intranation:%s" (get-keychain-password "znc-intranation")))
      (erc :server "brewdog.bradleywright.net" :port 60667 :nick "brad" :password (format "brad:%s" (get-keychain-password "znc-brad")))
)))

;; switch to ERC with Ctrl+c Ctrl+e
(global-set-key (kbd "C-c C-e") 'djcb-erc-start-or-switch) ;; ERC
