;;; magit-org-todos.el --- Add local todo items to the magit status buffer

;; Copyright (C) 2018 Daniel Ma <github.com/danielma>
;; Author: Daniel Ma
;; URL: http://github.com/danielma/magit-org-todos
;; Created: 2018
;; Version: 0.1.3
;; Keywords: org-mode magit tools
;; Package-Version: 0.1.3
;; Package-Requires: ((magit "2.0.0") (emacs "24"))

;;; Commentary:
;;
;; adds all TODO items from a todo.org file in the magit project's root
;; to the magit status buffer

;;; Code:
(require 'magit)
(require 'org-element)

;;; Customizations:
(defgroup magit-org-todos nil
  "Add local todo items to the magit status buffer"
  :group 'tools)

(defcustom magit-org-todos-filename "todo.org"
  "The org file that holds todo items."
  :group 'magit-org-todos
  :type 'string)

;;; Implementation:
(defun magit-org-todos--todo-file-path ()
  "Path of the todo file."
  (let* ((toplevel (magit-toplevel))
         (todo (concat toplevel magit-org-todos-filename)))
    todo))

(defun magit-org-todos--magit-visit-org-todo ()
  "Visits the org todo file."
  (interactive)
  (find-file (magit-org-todos--todo-file-path)))

(defvar magit-org-todos-section-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap magit-visit-thing] 'magit-org-todos--magit-visit-org-todo)
    m))

(defun magit-org-todos--handle-element (el)
  "Handle org elements when rendering the TODO list in magit."
  (cond ((not (listp el)) el)
        ;; TODO: Propertize this instead to provide clickable links.
        ((eq (car el) 'link) (concat (car (last el)) " "))
        ((eq (car el) 'verbatim)
         (let ((value (plist-get (cadr el) :value)))
           (concat (propertize value 'face 'org-verbatim) " ")))
        (t (error (format "Don't know how to parse: %s" el)))))

;;;###autoload
(defun magit-org-todos-insert-org-todos ()
  "Insert org todos from the local todo.org."
  (when (file-readable-p (magit-org-todos--todo-file-path))
    (let ((todos (with-temp-buffer
                   (insert-file-contents (magit-org-todos--todo-file-path))
                   (org-mode)
                   (org-element-map (org-element-parse-buffer) 'headline
                     (lambda (headline)
                       (let ((todo-type (org-element-property :todo-type headline)))
                         (and (eq todo-type 'todo) headline)))))))
      (magit-insert-section (org-todos-wrapper)
        (magit-insert-heading "Todos:")
        (dolist (todo todos)
          (let ((keyword (org-element-property :todo-keyword todo))
                (title (org-element-property :title todo))
                (priority (org-element-property :priority todo))
                (level (org-element-property :level todo)))
            (magit-insert-section (org-todos title)
              (dotimes (i level)
                (insert "*"))
              (insert (concat " " (propertize keyword 'face 'org-todo) " "))
              (when priority
                (insert (propertize (concat "[#" (string priority) "]") 'face 'org-priority)  " "))
              (dolist (el title)
                (insert (magit-org-todos--handle-element el)))
              (insert ?\n))))
        (insert ?\n)))))


(defun magit-org-todos--handle-element (el)
  "Handle org elements when rendering the TODO list in magit."
  (cond
   ((not (listp el)) el)
   ((eq (car el) 'link)
    (concat (car (last el)) " "))
   ((eq (car el) 'verbatim)
    (concat (propertize (plist-get (cadr el) :value) 'face 'org-verbatim) " "))
   (t (error "Don't know how to parse this element: %s" el))))

;;;###autoload
(defun magit-org-todos-insert-org-todos ()
  "Insert org todos from the local todo.org."
  (when-let ((todo-file-path (magit-org-todos--todo-file-path))
             (file-readable-p todo-file-path))
    (let ((todos (with-temp-buffer
                   (insert-file-contents todo-file-path)
                   (org-mode)
                   (org-element-map (org-element-parse-buffer) 'headline
                     (lambda (headline)
                       (when (eq (org-element-property :todo-type headline) 'todo)
                         headline))))))
      (magit-insert-section (org-todos-wrapper)
        (magit-insert-heading "Todos:")
        (dolist (todo todos)
          (let ((keyword (org-element-property :todo-keyword todo))
                (title (org-element-property :title todo))
                (priority (org-element-property :priority todo))
                (level (org-element-property :level todo)))
            (magit-insert-section (org-todos title)
              (insert (make-string level ?*))
              (insert " " (propertize keyword 'face 'org-todo) " ")
              (when priority
                (insert (propertize (format "[#%c]" priority) 'face 'org-priority) " "))
              (dolist (el title)
                (insert (magit-org-todos--handle-element el)))
              (insert ?\n))))
        (insert ?\n)))))


;;;###autoload
(defun magit-org-todos-autoinsert ()
  "Automatically insert todo section into magit status buffer."
  (magit-add-section-hook
   'magit-status-sections-hook
   'magit-org-todos-insert-org-todos
   'magit-insert-staged-changes
   t))

(provide 'magit-org-todos)

;;; magit-org-todos.el ends here
