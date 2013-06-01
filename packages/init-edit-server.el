(defun bw-switch-to-emacs ()
  "Switches to Emacs"
  (interactive)
  (do-applescript "set appName to \"Emacs\"
set needsActivation to false
tell application \"System Events\"
  if frontmost of process appName then
    set visible of process appName to false
  else
    set needsActivation to true
  end if
end tell


if needsActivation then
  tell application appName to activate
end if"))

(defun bw-switch-to-chrome ()
  "Switches to Chrome"
  (interactive)
  (do-applescript "set appName to \"Google Chrome\"
set needsActivation to false
tell application \"System Events\"
  if frontmost of process appName then
    set visible of process appName to false
  else
    set needsActivation to true
  end if
end tell


if needsActivation then
  tell application appName to activate
end if"))

(use-package edit-server
  :init
  (progn
    (edit-server-start)
    (setq edit-server-url-major-mode-alist
          '(("github\\.com" . markdown-mode)))
    (add-hook 'edit-server-start-hook 'bw-switch-to-emacs)
    (add-hook 'edit-server-done-hook 'bw-switch-to-chrome)))

(provide 'init-edit-server)
