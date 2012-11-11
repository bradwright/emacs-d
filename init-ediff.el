(setq
 ;; make two side-by-side windows
 ediff-split-window-function 'split-window-horizontally
 ;; ignore whitespace diffs
 ediff-diff-options          "-w"
 ;; Do everything in one frame always
 ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'init-ediff)
