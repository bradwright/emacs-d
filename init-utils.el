;;; -*- lexical-binding: t -*-

;;; My custom Emacs lisp functions

(require 'cl)
(require 'grep)

(defun bw-turn-on-auto-fill ()
  "Enables auto-fill"
  (interactive)
  (auto-fill-mode 1))

(defun bw-turn-off-auto-fill ()
  "Disables auto-fill"
  (interactive)
  (auto-fill-mode -1))

(defun bw-enable-hl-line-mode ()
  "Enables hl-line-mode"
  (interactive)
  (hl-line-mode 1))

(defun bw-turn-on-flymake-mode ()
  "Turns on flymake-mode locally"
  (interactive)
  (flymake-mode 1))

(defun bw/turn-on-electric-indent-mode ()
  "Turns on electric-indent-mode"
  (interactive)
  (electric-indent-mode 1))

(defun bw/turn-off-electric-indent-mode ()
  "Turns off electric-indent-mode"
  (interactive)
  (electric-indent-mode -1))

(defun bw-locate-library-dir (library)
  "Locates the directory containing a loaded library"
  (file-name-directory (locate-library library)))

(defun bw-add-to-load-path (dir)
  "Adds `dir` to load-path"
  (add-to-list 'load-path dir))

(defun bw-add-to-custom-theme-load-path (dir)
  "Adds `dir` to custom-theme-load-path"
  (add-to-list 'custom-theme-load-path dir))

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
  (if server-buffer-clients
      (server-edit)
    (cond ((y-or-n-p "Quit Emacs? ")
           (save-buffers-kill-terminal)))))

;; http://andrewcoxtech.blogspot.co.uk/2009/11/inserting-bom-into-file.html
(defun bw-insert-bom()
  "Inserts a valid UTF8 byte order mark"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ucs-insert (string-to-number "FEFF" 16))
    (message "BOM inserted")))

(defun bw-read-string-at-point ()
  "Grabs the `symbol` currently at point"
  (interactive)
  (symbol-name (symbol-at-point)))

(defun bw-is-image (name)
  (member (file-name-extension name) '("jpg" "png" "gif" "jpeg")))

(defun bw-completing-read (prompt index)
  (ido-completing-read prompt index)
  ;;(grizzl-completing-read prompt (grizzl-make-index index))
  )

(defun bw-evil-escape-if-next-char (c)
  "Watches the next letter.  If c, then switch to Evil's normal mode; otherwise insert a k and forward unpressed key to unread-command events"
  (self-insert-command 1)
  (let ((next-key (read-event)))
    (if (= c next-key)
        (progn
          (delete-backward-char 1)
          (do-evil-esc))
      (setq unread-command-events (list next-key)))))

(defun bw-evil-escape-if-next-char-is-j (arg)
  "Wrapper around escape-if-next-char and the character j"
  (interactive "p")
  (if (= arg 1)
      (bw-evil-escape-if-next-char ?j)
    (self-insert-command arg)))

(defun bw-open-term (&optional arg)
  "Opens an ansi-term with value of $TERM - force new ansi-term
with prefix"
  (interactive "p")
  (if (or (not (get-buffer "*ansi-term*")) (= arg 4))
      (ansi-term (getenv "SHELL"))
    (switch-to-buffer "*ansi-term*")))

(defun bw-eproject-ido-switch-buffers ()
  "Like ido-switch-buffer, but for the current eproject."
  (interactive)
  (if (not (eproject-root))
      (error "No active project was found")
    (switch-to-buffer
     (bw-completing-read
      (concat (eproject-name) " buffers: ")
      (mapcar #'buffer-name (--eproject-buffers))))))

(defun --eproject-buffers ()
  (when (eproject-root)
    (cdr (assoc (eproject-root) (eproject--project-buffers)))))

(defun bw-switch-to-scratch ()
  "Switch to the scratch buffer if it exists"
  (interactive)
  (switch-to-buffer "*scratch*"))


;; Gotten from:
;; http://whattheemacsd.com/appearance.el-01.html
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(provide 'init-utils)
