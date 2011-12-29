;; Load erlang mode

;; find the `erl` executable
;; we use file-truename as it's probably symlinked
(setq erlang-bin
      (file-truename
       (executable-find "erl")))

;; traverse up the tree to find the "root" directory
(setq erlang-root-dir
      (file-truename
       (concat
        (file-name-directory
         erlang-bin)
        "../../.."))) ;; this relative stuff might be different on Linux

(when (file-exists-p erlang-root-dir)
  (add-to-list
   'load-path
   (car (file-expand-wildcards
         (concat erlang-root-dir "/lib/erlang/lib/tools-*/emacs"))))
  (add-to-list 'exec-path (concat erlang-root-dir "/bin"))
  (setq erlang-man-root-dir (concat erlang-root-dir "/share/man"))
  (require 'erlang-start)

  ;; distel
  (add-to-list 'load-path (concat vendor-dotfiles-dir "/distel/elisp"))
  (require 'distel)
  (distel-setup))

(setq auto-mode-alist
  (append auto-mode-alist
    '(("\\.rel$" . erlang-mode)
      ("\\.app$" . erlang-mode)
      ("\\.appSrc$" . erlang-mode)
      ("\\.app.src$" . erlang-mode)
      ("rebar.config" . erlang-mode)
      ("sys.config" . erlang-mode)
      ("app.config" . erlang-mode)
      ("Emakefile" . erlang-mode)
      ("\\.hrl$" . erlang-mode)
      ("\\.erl$" . erlang-mode)
      ("\\.yrl$" . erlang-mode))))
