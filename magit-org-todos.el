;;; magit-org-todos --- Add local todo items to the magit status buffer

;; Copyright (C) 2018 Daniel Ma <github.com/danielma>
;; Author: Daniel Ma
;; URL: http://github.com/danielma/magit-org-todos
;; Created: 2018
;; Version: 1.0
;; Keywords: org magit todo
;; Package-Requires: ((magit) (org))

;;; Commentary:
;;
;; adds all TODO items from a todo.org file in the magit project's root
;; to the magit status buffer

;;; Code:
(require 'magit)
(require 'org)

(defun mot--todo-file-path ()
  "Path of the todo file."
  (let* ((toplevel (magit-toplevel))
         (todo (concat toplevel "todo.org")))
    todo))

(defun magit-org-todos/insert-org-todos ()
  "Insert org todos from the local todo.org."
  (when (file-readable-p (mot--todo-file-path))
    (let ((todos (with-temp-buffer
                   (insert-file-contents (mot--todo-file-path))
                   (org-mode)
                   (org-element-map (org-element-parse-buffer) 'headline
                     (lambda (headline)
                       (let ((todo-type (org-element-property :todo-type headline)))
                         (and (eq todo-type 'todo) headline)))))))
      (magit-insert-section (org-todos)
        (magit-insert-heading "Todos:")
        (dolist (todo todos)
          (let ((keyword (org-element-property :todo-keyword todo))
                (title (org-element-property :raw-value todo)))
            (magit-insert-section (org-todo title)
            (insert todo)
            (insert ?\n))))
        (insert ?\n)))))

(defun mot--magit-visit-org-todo ()
  "Visits the org todo file."
  (interactive)
  (find-file (mot--todo-file-path)))

(defvar magit-org-todo-section-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap magit-visit-thing] 'mot--magit-visit-org-todo)
    m))

(provide 'magit-org-todos)

;;; magit-org-todos.el ends here
