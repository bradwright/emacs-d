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

(defun turn-on-flymake-mode ()
  "Turns on flymake-mode locally"
  (interactive)
  (flymake-mode 1))

(defun turn-on-paredit ()
  "Turn paredit-mode on locally"
  (paredit-mode 1))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str))) str)

(defun get-keychain-password (account-name)
  "Gets `account` keychain password from OS X Keychain"
  (chomp
   (shell-command-to-string
    (concatenate
     'string
     "security 2>&1 >/dev/null find-generic-password -ga "
     account-name
     "| sed 's/^password: \\\"\\(.*\\)\\\"/\\1/'"))))

(defun get-buffer-line-length ()
  "Counts the number of lines in the current buffer"
  (count-lines (point-min) (point-max)))

(defun change-mode-if-not-in-mode (mode)
  "Changes to a mode if we're not already in that mode"
  (when (not (eq mode major-mode))
    (funcall mode)))


;; next 4 defuns from: http://paste.lisp.org/display/111574

(defun env-line-to-cons (env-line)
  "convert a string of the form \"VAR=VAL\" to a
cons cell containing (\"VAR\" . \"VAL\")."
  (if (string-match "\\([^=]+\\)=\\(.*\\)" env-line)
    (cons (match-string 1 env-line) (match-string 2 env-line))))

(defun interactive-env-alist (&optional shell-cmd env-cmd)
  "launch /usr/bin/env or the equivalent from an interactive
shell, parsing and returning the environment as an alist."
  (interactive)
  (let ((cmd (concat (or shell-cmd "/bin/bash -ic")
                     " "
                     (or env-cmd "/usr/bin/env"))))
    (mapcar 'env-line-to-cons
            (remove-if
             (lambda (str)
               (string-equal str ""))
             (split-string (shell-command-to-string cmd) "[\r\n]")))))

(defun setenv-from-cons (var-val)
  "set an environment variable from a cons cell containing
two strings, where the car is the variable name and cdr is
the value, e.g. (\"VAR\" . \"VAL\")"
  (setenv (car var-val) (cdr var-val)))

(defun setenv-from-shell-environment (&optional shell-cmd env-cmd)
  "apply the environment reported by `/usr/bin/env' (or env-cmd)
as launched by `/bin/bash -ic' (or shell-cmd) to the current
environment."
  (mapc 'setenv-from-cons (interactive-env-alist shell-cmd env-cmd)))

;; hook related functions, since anonymous functions can't be
;; guaranteed to not be added multiple times
(defun magit-fill-column ()
  (setq fill-column 72))

(defun bw-turn-on-auto-fill ()
  (turn-on-auto-fill))

(defun bw-clojure-repl-program ()
  "Changes lisp function to use Leiningen repl"
  (setq inferior-lisp-program "lein repl"))

(defun bw-clojure-slime-repl-font-lock ()
  "Gives us Clojure font lock in the repl"
  (let (font-lock-mode)
    (clojure-mode-font-lock-setup)))
