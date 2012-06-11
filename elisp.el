;;; -*- lexical-binding: t -*-

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Recently: " recentf-list nil t)))
    (when file
      (find-file file))))

;; gotten from:
;; http://stackoverflow.com/questions/12492/pretty-printing-xml-files-on-emacs
(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this. The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end)))

(defun bw-start-term (buffer-name)
  "Start a terminal and rename buffer."
  (interactive "sbuffer name: ")
  (ansi-term "/bin/bash")
  (rename-buffer (concat "*term: " buffer-name "*") t))

(defun local-hl-line-mode-off ()
  "Turn hl-line-mode off locally to a buffer"
  (interactive)
  (hl-line-mode -1))

(defun local-hl-line-mode-on ()
  "Turn hl-line-mode off locally to a buffer"
  (interactive)
  (make-local-variable 'global-hl-line-mode)
  (hl-line-mode))

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
  (interactive "sAccount name: ")
  (chomp
   (shell-command-to-string
    (concat
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

(defun env-var-from-login-shell (var)
  "Fetches a named variable from a login shell"
  (interactive "sENV variable: ")
  (let ((command-to-run (concat "$SHELL -l -c 'echo $" var "'")))
    (chomp (shell-command-to-string command-to-run))))

;; next 4 defuns from: http://paste.lisp.org/display/111574
(defun env-line-to-cons (env-line)
  "convert a string of the form \"VAR=VAL\" to a
cons cell containing (\"VAR\" . \"VAL\")."
  (if (string-match "\\([^=]+\\)=\\(.*\\)" env-line)
    (cons (match-string 1 env-line) (match-string 2 env-line))))

(defun interactive-env-alist (&optional shell-cmd env-cmd)
  "launch /usr/bin/env or the equivalent from an interactive
shell, parsing and returning the environment as an alist."
  (let ((cmd (concat (or shell-cmd "/bin/bash -ic")
                     " "
                     (or env-cmd "/usr/bin/env"))))
    (mapcar 'env-line-to-cons
            (remove-if
             (lambda (str)
               (string-equal str ""))
             (split-string (shell-command-to-string cmd) "[\r\n]")))))

;; modified this function to be a bit more readable
(defun setenv-from-cons (var-val)
  "set an environment variable from a cons cell containing
two strings, where the car is the variable name and cdr is
the value, e.g. (\"VAR\" . \"VAL\")"
  (let ((key (car var-val))
        (val (cdr var-val)))
    ;; for whatever reason this can be nil
    (when (not (eq key nil))
      (setenv key val))))

(defun setenv-from-shell-environment (&optional shell-cmd env-cmd)
  "apply the environment reported by `/usr/bin/env' (or env-cmd)
as launched by `/bin/bash -ic' (or shell-cmd) to the current
environment."
  (mapc 'setenv-from-cons (interactive-env-alist shell-cmd env-cmd)))

;; hook related functions, since anonymous functions can't be
;; guaranteed to not be added multiple times
(defun bw-fill-column ()
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

;; http://paste.lisp.org/display/129008
(defun quit-or-hide ()
  (interactive)
  (if (boundp 'server-name)
      (if (> (length server-clients) 1)
          (delete-frame)
        (make-frame-invisible nil t))
    (bw-kill-emacs)))

(defun bw-kill-emacs ()
  "Warn before exiting Emacs"
  (interactive)
  (cond ((y-or-n-p "Quit Emacs? ")
         (save-buffers-kill-emacs))))

(defun bw-turn-off-trailing-whitespace ()
  "Turns off trailing whitespace"
  (make-local-variable 'whitespace-line-column)
  (setq whitespace-line-column nil)
  (whitespace-mode -1))

(defun my-insert-tab (&optional arg)
  "inserts just whitespace"
  (interactive "P")
  (insert-tab arg))

;; from: http://stackoverflow.com/a/7934783
(defun beautify-json ()
  "Indents and pretties JSON structures"
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

(defun bw-load-mode-files (&optional load-from)
  "Loads all files resident in the `modes` directory"
  (let ((modes-dir (or load-from (concat dotfiles-dir "modes"))))
    (mapc 'load (directory-files modes-dir t "^[^#].*el$"))))
