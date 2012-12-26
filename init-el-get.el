;; el-get configuration

(defconst package-base-dir
  (bw-join-dirs dotfiles-dir "packages"))
(make-directory package-base-dir t)
(bw-add-to-load-path package-base-dir)

(defconst el-get-base-dir
  (bw-join-dirs package-base-dir "el-get/installed"))

(make-directory el-get-base-dir t)
(setq el-get-dir el-get-base-dir)

;; if el-get is already installed it'll live here
(bw-add-to-load-path (bw-join-dirs el-get-base-dir "el-get"))

(eval-after-load 'el-get
  '(progn
     (add-to-list 'el-get-recipe-path (bw-join-dirs package-base-dir "el-get/recipes"))
     ;; only take the latest commit and not the whole history
     (setq el-get-git-shallow-clone t)))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let ((el-get-master-branch)
          ;; skip EmacsWiki recipes
          (el-get-install-skip-emacswiki-recipes))
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq bw-packages
      '(use-package ; this has to come first due to the way my config
                    ; is set up
         ack-and-a-half
         browse-kill-ring
         color-theme-solarized
         csv-mode
         diminish
         el-get
         eproject
         expand-region
         flymake-cursor
         git-modes
         ido-ubiquitous
         idomenu
         js2-mode
         json-mode
         magit
         markdown-mode
         multiple-cursors
         paredit
         rhtml-mode
         scss-mode
         smex
         undo-tree
         web-mode
         xterm-frobs
         yasnippet))

(if *is-a-mac*
    (progn
      (add-to-list 'bw-packages 'exec-path-from-shell)
      (add-to-list 'bw-packages 'edit-server)))

(el-get 'sync bw-packages)

(provide 'init-el-get)
