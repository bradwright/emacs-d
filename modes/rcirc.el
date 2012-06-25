;;; -*- lexical-binding: t -*-

;; rcirc configuration

(setq rcirc-server-alist
      `(("brewdog.bradleywright.net" :nick "intranation"
         :password ,(concat "intranation:" (get-keychain-password "znc-intranation"))
         :port 60667)
        ("brewdog.bradleywright.net" :nick "brad"
         :password ,(concat "brad:" (get-keychain-password "znc-brad"))
         :port 60667)))

(defun rcirc-detach-buffer ()
  (interactive)
  (let ((buffer (current-buffer)))
    (when (and (rcirc-buffer-process)
           (eq (process-status (rcirc-buffer-process)) 'open))
      (with-rcirc-server-buffer
    (setq rcirc-buffer-alist
          (rassq-delete-all buffer rcirc-buffer-alist)))
      (rcirc-update-short-buffer-names)
      (if (rcirc-channel-p rcirc-target)
      (rcirc-send-string (rcirc-buffer-process)
                 (concat "DETACH " rcirc-target))))
    (setq rcirc-target nil)
    (kill-buffer buffer)))

(define-key rcirc-mode-map (kbd "C-c C-d") 'rcirc-detach-buffer)
