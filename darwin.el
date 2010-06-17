;; OS X Specific configuration

;; When we have a window system, do some specific stuff
(when window-system
  (progn
    ;; set my favourite Mac font as the default font
    (set-face-font 'default "-apple-inconsolata-medium-r-normal--15-150-72-72-m-150-iso10646-1"))
  ;; Free up the option key for special characters
  (setq ns-alternate-modifier (quote none))
  (setq ns-command-modifier (quote meta)))

;;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Edit path crap
(setenv "PATH" (concat "/Users/bradleyw/bin:/Users/bradleyw/Projects/homebrew/bin:"(getenv "PATH")))
(setq magit-git-executable "/Users/bradleyw/Projects/homebrew/bin/git")
