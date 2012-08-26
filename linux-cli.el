;;; -*- lexical-binding: t -*-

;;; Linux CLI changes

;; if the terminal supports 256 or more, load Solarized Dark
(if (>= (display-color-cells) 256)
    (load-solarized-theme))
