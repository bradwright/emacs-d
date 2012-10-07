;;; -*- lexical-binding: t -*-

;;; Linux CLI changes

;; if the terminal supports 256 or more, load Solarized Dark
(if (or (>= (display-color-cells) 256) (eq (display-color-cells) 16))
    (load-solarized-theme))
