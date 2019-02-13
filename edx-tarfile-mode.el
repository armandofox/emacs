
(setq edx-nav-stack '())

(defun edx-pathname-from-xml-element (elt)
  (if (string-match  "<\\(\\sw+\\).*\"\\(\\S-+\\)\"" elt)
      (concat (file-name-directory (buffer-file-name (current-buffer)))
              "../"
              (match-string 1 elt) ;; type of elt - vertical, etc
              "/"
              (match-string 2 elt)
              ".xml")))

(defun edx-visit (file other-window)
  (if other-window
      (find-file-other-window file)
    (find-file file)))

(defun edx-navigate-to-element-on-line (other-window)
  (interactive "P")
  (let ((file (edx-pathname-from-xml-element (thing-at-point 'line))))
    (setq edx-nav-stack (cons file edx-nav-stack))
    (edx-visit file other-window)))

(defun edx-up-element (other-window)
  (interactive "P")
  (let ((file (car edx-nav-stack)))
    (setq edx-nav-stack (cdr edx-nav-stack))
    (edx-visit file other-window)))

(global-set-key "\M-[" 'edx-navigate-to-element-on-line)
(global-set-key "\M-]" 'edx-up-element)
