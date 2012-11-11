;; use-package
;; https://github.com/jwiegley/use-package

;; This is used to asynchronously load and package mode configuration

(require 'use-package)

(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))

(provide 'init-use-package)
