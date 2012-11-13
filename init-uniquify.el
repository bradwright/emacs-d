(require 'uniquify)
;; this shows foo/bar and baz/bar when two files are named bar
(setq uniquify-buffer-name-style 'forward)
;; strip common buffer suffixes
(setq uniquify-strip-common-suffix t)
;; re-uniquify buffer names after killing one
(setq uniquify-after-kill-buffer-p t)

(provide 'init-uniquify)
