;;; -*- lexical-binding: t -*-

;;; My custom Emacs lisp functions

(defun bw-enable-hl-line-mode ()
  "Enables hl-line-mode"
  (interactive)
  (hl-line-mode 1))

(defun bw-turn-on-flymake-mode ()
  "Turns on flymake-mode locally"
  (interactive)
  (flymake-mode 1))

(defun bw-locate-library-dir (library)
  "Locates the directory containing a loaded library"
  (file-name-directory (locate-library library)))

(defun bw-add-to-load-path (dir)
  "Adds `dir` to load-path"
  (add-to-list 'load-path dir))

(defun bw-join-dirs (prefix suffix)
  "Joins `prefix` and `suffix` into a directory"
  (file-name-as-directory (concat prefix suffix)))

(defun bw-turn-on-subword-mode ()
  "Turns on subword mode for a buffer"
  (subword-mode 1))

;; from: http://stackoverflow.com/a/7934783
(defun beautify-json ()
  "Indents and pretties JSON structures"
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

(defun bw-occur-non-ascii-chars ()
  "Finds characters that aren't in the displayable range for ASCII"
  (interactive)
  (occur "[^\000-\177]"))

;; from:
;; https://github.com/technomancy/emacs-starter-kit/blob/31c2465712485a54aba6a3ef6d1bef9b564f8f37/starter-kit-defuns.el#L179
(defun sudo-edit (&optional arg)
  "Edit this file as sudo"
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo::" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo::" buffer-file-name))))

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

(defun bw-require-list (items)
  "Takes a list of items to require"
  (interactive)
  (dolist (item items)
    (require `,item nil t)))

;; http://paste.lisp.org/display/129008
(defun quit-or-hide ()
  (interactive)
  (if (boundp 'server-name)
      (if (> (length server-clients) 1)
          (delete-frame)
        (make-frame-invisible nil t))
    (bw-kill-emacs)))

(defun bw-kill-emacs ()
  "If this buffer is a client, just kill it, otherwise confirm
the quit."
  (interactive)
  (if (frame-parameter (selected-frame) 'client)
      (save-buffers-kill-terminal)
    (cond ((y-or-n-p "Quit Emacs? ")
           (save-buffers-kill-terminal)))))

;; http://andrewcoxtech.blogspot.co.uk/2009/11/inserting-bom-into-file.html
(defun bw-insert-bom()
  "Inserts a valid UTF8 byte order mark"
  (interactive)
  (goto-char (point-min))
  (ucs-insert (string-to-number "FEFF" 16)))

(provide 'init-utils)
