;;; Linux stuff
(when *is-linux*
  (progn

    ;; remote copy/paste from host OS X machine
    (defun copy-from-remote ()
      (shell-command-to-string "~/bin/rpbpaste"))

    (defun paste-to-remote (text &optional push)
      "Copies the top of the kill ring stack to the OSX clipboard"
      (let ((process-connection-type nil))
        (let ((proc (start-process "rpbcopy" "*Messages*" "~/bin/rpbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))
    (setq interprogram-paste-function 'copy-from-remote)
    (setq interprogram-cut-function 'paste-to-remote)))

(provide 'init-linux)
