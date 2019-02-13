;;;
;;; Hooks for find-file
;;;

(require 'cl)

(defun find-makefile-hook ()
  (when (string-match "\\(GNU\\)?[Mm]akefile"
                      (buffer-file-name (current-buffer)))
    (makefile-mode)))

(push 'find-makefile-hook find-file-hooks)

