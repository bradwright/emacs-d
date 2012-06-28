;;; -*- lexical-binding: t -*-

;; Ruby mode

;; this variable is stupid - apparently Ruby needs its own indent
;; variable
;; 2-space indent is idiomatic
(setq ruby-indent-level 2)

(add-to-list 'auto-mode-alist '("[vV]agrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.css\\.erb$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\.erb$" . scss-mode))

;; make sure we can find Ruby files
(eval-after-load 'find-file-in-project '(add-to-list 'ffip-patterns "*.rake"))
(eval-after-load 'find-file-in-project '(add-to-list 'ffip-patterns "*Gemfile"))
(eval-after-load 'find-file-in-project '(add-to-list 'ffip-patterns "*Rakefile"))

;; implicit rails project mode
(eval-after-load 'eproject '(require 'eproject-ruby-on-rails))

(defun find-rails-file ()
  "Finds a Rails file"
  (interactive)
  (when (eq (eproject-type) 'ruby-on-rails)
    (find-file
     (concat
      (eproject-root)
      (ido-completing-read
       "File: "
       (mapcar
        (lambda (e)
          (replace-regexp-in-string (eproject-root) "" e))
        (eproject-list-project-files)))))))

;; TODO: can we factor this out into a macro?
(defun find-rails-type (predicate title)
  "Finds a file filtered by predicate"
  (when (eq (eproject-type) 'ruby-on-rails)
    (find-file
     (concat
      (eproject-root)
      (ido-completing-read
       title
       (mapcar
        (lambda (e)
          (replace-regexp-in-string (eproject-root) "" e))
        (remove-if-not predicate (eproject-list-project-files))))))))

(defun find-controllers-rails ()
  "Finds controller files in an eproject-rails project"
  (interactive)
  (find-rails-type 'is-controller "Controller: "))

(defun find-models-rails ()
  "Finds model files in an eproject-rails project"
  (interactive)
  (find-rails-type 'is-model "Model: "))

(defun find-views-rails ()
  "Finds view files in an eproject-rails project"
  (interactive)
  (find-rails-type 'is-view "View: "))

(defun is-controller (name)
  "Filters a string"
  (interactive "sController: ")
  (not (equal (string-match "app/controller*" name) nil)))

(defun is-model (name)
  (interactive "sModel: ")
  (not (equal (string-match "app/models*" name) nil)))

(defun is-view (name)
  (interactive "sView: ")
  (not (equal (string-match "app/views*" name) nil)))
