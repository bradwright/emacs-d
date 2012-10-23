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
  (set (make-local-variable 'whitespace-line-column) nil)
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

(defun bw-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
            (unless (integerp x)
              (push (prin1-to-string x t) tag-names)))
          tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

;; customise solarized-dark/light themes
(defadvice load-theme
  (before load-theme)
  (let ((theme-name (ad-get-arg 0)))
    (when (or (eq theme-name 'solarized-dark)
              (eq theme-name 'solarized-light))
      (progn
        (custom-set-faces
         '(magit-diff-add ((t (:inherit diff-added :weight normal))))
         '(magit-diff-del ((t (:inherit diff-removed :weight normal))))
         '(diff-refine-change ((t (:inherit diff-refine-change :background nil))))
         '(iedit-occurrence ((t (:inherit lazy-highlight))))
         '(match ((t (:inherit lazy-highlight :reverse t))))
         '(erb-face ((t (:background nil))))
         '(erb-out-delim-face ((t (:inherit erb-exec-delim-face :foreground "#b58900")))))
        (when (and (display-graphic-p) *is-a-mac*)
          ;; My Macs have the --srgb flag set
          (setq solarized-broken-srgb nil))))))

(ad-activate 'load-theme)

(defun bw-toggle-solarized ()
  "Toggles between solarized light and dark"
  (interactive)
  (cond
   ((custom-theme-enabled-p 'solarized-dark)
    (progn
      (disable-theme 'solarized-dark)
      (enable-theme 'solarized-light)))
   ((custom-theme-enabled-p 'solarized-light)
    (progn
      (disable-theme 'solarized-light)
      (enable-theme 'solarized-dark)))))

(defun bw-locate-library-dir (library)
  "Locates the directory containing a loaded library"
  (file-name-directory (locate-library library)))

(defun bw-add-to-load-path (dir)
  (add-to-list 'load-path dir))

(defun bw-join-dirs (prefix suffix)
  (file-name-as-directory (concat prefix suffix)))

;; from:
;; https://github.com/technomancy/emacs-starter-kit/blob/31c2465712485a54aba6a3ef6d1bef9b564f8f37/starter-kit-defuns.el#L179
(defun sudo-edit (&optional arg)
  "Edit this file as sudo"
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo::" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo::" buffer-file-name))))

(defun turn-on-subword-mode ()
  "Turns on subword-mode"
  (subword-mode 1))

(defun bw-shorten-dir (dir)
  "Shortens a directory path to e.g ~/src"
  (replace-regexp-in-string (getenv "HOME") "~" dir))

(defun bw-iedit-defun ()
  "Calls iedit with a prefix of 0"
  (interactive)
  (let ((current-prefix-arg '(0)))
    (call-interactively 'iedit-mode)))

(defun bw-occur-invalid-chars ()
  "Finds characters that aren't in the displayable range for ASCII"
  (interactive)
  (occur "[^\000-\177]"))
