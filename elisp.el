(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to the same value used by the user shell"
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; gotten from:
;; http://stackoverflow.com/questions/12492/pretty-printing-xml-files-on-emacs
(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

(defun bw-start-term (buffer-name)
  "Start a terminal and rename buffer."
  (interactive "sbuffer name: ")
  (ansi-term "/bin/bash")
  (rename-buffer (concatenate 'string "*term: " buffer-name "*") t))

(defun local-hl-line-mode-off ()
  "Turn hl-line-mode off locally to a buffer"
  (interactive)
  (make-local-variable 'global-hl-line-mode)
  (setq global-hl-line-mode nil))

(defun turn-on-paredit ()
  "Turn paredit-mode on locally"
  (paredit-mode 1))

(defun get-keychain-password (account-name)
  "Gets `account` keychain password from OS X Keychain"
  (shell-command-to-string
   (concatenate
    'string
    "security 2>&1 >/dev/null find-generic-password -ga "
    account-name
    "| sed 's/^password: \\\"\\(.*\\)\\\"/\\1/'")))
