;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  HAML mode customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'change-many)

(setq haml-mode-hook
      '(lambda ()
         (auto-fill-mode 0)))

(defun hamlize-region ()
  "Try to convert some RHTML to HAML."
  (interactive)
  (save-excursion
    (narrow-to-region (point) (mark))
    (beginning-of-buffer)
    (change-many '(("</\\sw+>" "")
                   ("<\\(\\sw+\\)>\\s-*" "%\\1 ")
                   ("<%=" "=")
                   ("%>" "")))))
  
