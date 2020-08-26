;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Generally useful functions used all over the place, including inside my
;;;  major mode hooks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun tags-search-other-window (regexp)
  "Search for tag (regexp) in other window"
  (interactive "sTags search other window (regexp): ")
  (switch-to-buffer-other-window nil)
  (tags-search regexp))


(defun uuid ()
  (interactive)
  (insert "\"")
  (call-process "uuidgen" nil t)
  (backward-char 1) (delete-char 1)                       ;delete the newline
  (insert "\","))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Number lines in region starting with 1 or prefix arg
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun number-lines-region (firstnum begin end)
  "Prepend line number to each line in region; prefix arg is number of first
line.  Non-interactive args are FIRSTNUM BEGIN END."
  (interactive "p\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (beginning-of-buffer)
      (while (not (save-excursion (end-of-line) (eobp)))
        (beginning-of-line)
        (insert (format "%3d " firstnum))
        (forward-line 1)
        (setq firstnum (1+ firstnum))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Choose n'th regexp from a file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nth-regexp (num regexp file)
  "Searches for NUM'th occurrence of REGEXP in FILE and returns it as a string."
  (interactive "nNumber: \nsRegexp: \nfFile: ")
  (let ((temp-buffer (get-buffer-create "*temp*")))
    (save-excursion
      (switch-to-buffer temp-buffer)
      (erase-buffer)
      (insert-file-contents file)
      (beginning-of-buffer)
      (if (re-search-forward regexp (point-max) t num)
          (buffer-substring (match-beginning 0) (match-end 0))
        ""))))


(defun replace-on-matching-lines (min max regexp from to)
  "On lines matching REGEXP, replace-regexp FROM by TO."
  (interactive
   "r
sSelect lines containing regexp:
sString to replace on those lines:
sReplace with:")
  (require 'cl)
  (save-excursion
    (save-restriction
      (narrow-to-region min max)
      (goto-char (point-max))
      (while (not (bobp))
        (beginning-of-line)
        (setq p (point))
        (end-of-line)
        (setq line (buffer-substring p (point)))
        (if (string-match regexp line)
            (save-restriction
              (narrow-to-region p (point))
              (beginning-of-line)
              (replace-regexp from to)))
        (forward-line -1)))))

(defun see-chars ()
  "Displays characters typed, terminated by a 3-second timeout."
  (interactive)
  (let ((chars "")
        (inhibit-quit t))
    (message "Enter characters, terminated by 3-second timeout.")
    (while (not (sit-for 3))
      (setq chars (concat chars (list (read-char)))
            quit-flag nil))             ; quit-flag maybe set by C-g
    (message "Characters entered: %s" (key-description chars))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Search thru completions buffer during minibuffer completion (!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key minibuffer-local-completion-map "\C-s" 'isearch-completions)
(define-key minibuffer-local-must-match-map "\C-s" 'isearch-completions)

(defun isearch-completions ()
  "Do an isearch in the *Completions* buffer, grabbing anything near point
when the search is terminated, and putting it in the minibuffer. "
  (interactive)
  (let ((currwin (selected-window))
        (compwin (get-buffer-window " *Completions*"))
        found)
                                        ; If there isn't a completions buffer,
                                        ; make one.
    (or compwin
        (progn
          (minibuffer-complete)
          (setq compwin (get-buffer-window " *Completions*"))))

    (if (null compwin)
        nil
      (unwind-protect
          (progn
            (select-window compwin)
            (call-interactively 'isearch-forward)
            (let ((start (progn (skip-chars-backward "^\n \t")
                                (point)))
                  (end (progn (skip-chars-forward "^\n \t")
                              (point))))
              (setq found (buffer-substring start end))))
        (select-window currwin))
      (erase-buffer)
      (insert found))))
(define-key minibuffer-local-completion-map "\C-s" 'isearch-completions)
(define-key minibuffer-local-must-match-map "\C-s" 'isearch-completions)

(defun isearch-completions ()
  "Do an isearch in the *Completions* buffer, grabbing anything near point
when the search is terminated, and putting it in the minibuffer. "
  (interactive)
  (let ((currwin (selected-window))
        (compwin (get-buffer-window " *Completions*"))
        found)
                                        ; If there isn't a completions buffer,
                                        ; make one.
    (or compwin
        (progn
          (minibuffer-complete)
          (setq compwin (get-buffer-window " *Completions*"))))

    (if (null compwin)
        nil
      (unwind-protect
          (progn
            (select-window compwin)
            (call-interactively 'isearch-forward)
            (let ((start (progn (skip-chars-backward "^\n \t")
                                (point)))
                  (end (progn (skip-chars-forward "^\n \t")
                              (point))))
              (setq found (buffer-substring start end))))
        (select-window currwin))
      (erase-buffer)
      (insert found))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; From: jf41+@andrew.cmu.edu (Jonathan R. Ferro)
;;; Newsgroups: gnu.emacs.help
;;;Subject: Re: Paging through the completion buffer
;;;
;;; Since C-v and M-v are useless in the minibuffer, this little hack causes
;;;them to scroll the *Completions* buffer instead.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scroll-completions-down (&optional arg)
  (interactive "p")
  (save-excursion
    (let ((currwin (selected-window))
          (compwin (get-buffer-window " *Completions*")))
      (if compwin
          (unwind-protect
              (progn
                (select-window compwin)
                (call-interactively 'scroll-down))
            (select-window currwin))))))

(defun scroll-completions-up (&optional arg)
  (interactive "p")
  (save-excursion
    (let ((currwin (selected-window))
          (compwin (get-buffer-window " *Completions*")))
      (if compwin
          (unwind-protect
              (progn
                (select-window compwin)
                (call-interactively 'scroll-up))
            (select-window currwin))))))

(define-key minibuffer-local-completion-map "\C-v" 'scroll-completions-up)
(define-key minibuffer-local-completion-map "\M-v" 'scroll-completions-down)
(define-key minibuffer-local-must-match-map "\C-v" 'scroll-completions-up)
(define-key minibuffer-local-must-match-map "\M-v" 'scroll-completions-down)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shell command on buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shell-command-on-buffer (arg cmd)
  "Shell command on buffer.  With prefix arg, replace buffer with output of
shell command."
  (interactive "P
sShell command on buffer:")
  (shell-command-on-region (point-min) (point-max) cmd arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make C-x t toggle truncation in the buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key ctl-x-map "t" '(lambda () (interactive)
                             (setq truncate-lines (not truncate-lines))
                             (recenter)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Adjust horizontal scroll if you type off the edge of a truncated window.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default horizontal-adjust-always nil)
(make-variable-buffer-local 'horizontal-adjust-always)
(defun horizontal-adjust ()
  "This function checks if the actual point is within the horizontal visibility
of its window area, i.e. if point is beyond the left respectively right border.
If so, and if the local variable horizontal-adjust-always is non-nil,
then the window is scrolled left respectively right horizontally with a
minimum of 10 columns (but at least so that actual point position is visible)."
  (when horizontal-adjust-always
    (let ((column (- (current-column) (max (window-hscroll) 1))))
      (if (and (< column 0) (> (window-hscroll) 0))
          (scroll-right (max (- column) 10))
        (if (>= column (- (window-width) 2))
            (scroll-left (max (- column (window-width) -3) 10)))))))
;;
(add-hook 'post-command-hook 'horizontal-adjust t) ; install following function

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   drag mode line dynamically with mouse (emacs19)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mode-line-resize-dynamically ()
  "Resize a window by dragging the mode-line.
This must be bound to a mouse-down event in the mode-line."
  (interactive "@")
  (let* ((mouse (mouse-position))
         (start-frame (car mouse))
         (prev-y (cdr (cdr mouse)))
         (next (next-window)))
    (track-mouse
      (while (and (eq (car-safe (read-event)) 'mouse-movement)
                  (eq next (next-window)))
        (let* ((mouse (mouse-position))
               (frame (car mouse))
               (new-y (cdr (cdr mouse)))
               (delta (- new-y prev-y)))
          (cond ((and (eq frame start-frame)
                      (> (+ delta (window-height (selected-window)))
                         window-min-height))
                 (enlarge-window delta)
                 (setq prev-y new-y))))))))
(global-set-key [mode-line down-mouse-2]  'mode-line-resize-dynamically)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Create a list of numbers given start and end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-sequence (start end)
  (let ((l (make-list (1+ (- end start)) 0)))
    (dotimes (i (1+ (- end start)))
      (setf (nth i l) (+ i start)))
    l))

(defun space-concat (x y)
  (concat x " " y))

(defun make-string-sequence (start end)
  (let ((l (make-sequence start end)))
    (reduce 'space-concat (cdr l)
          :initial-value (format "%d" (car l)))))

(defun toggle-hilit (arg)
  (interactive "p")
  (cond
   ((eq hilit-background-mode 'light)
    (setq hilit-background-mode 'dark))
   ((eq hilit-background-mode 'dark)
    (setq hilit-background-mode nil))
   ((null hilit-background-mode)
    (setq hilit-background-mode 'light)))
  (load-library "hilit19")
  (hilit-rehighlight-buffer))

(global-set-key [f5] 'toggle-hilit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; turn the region into something suitable for pasting into Word or other
;;; non-ascii word processors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun change-many (change-list)
  (save-excursion
    (dolist (subst change-list)
      (goto-char (point-min))
      (while (re-search-forward (car subst) (point-max) t)
        (replace-match (cadr subst) nil nil)))))
(provide 'change-many)

(defun wordify-region ()
  "Nondestructively convert region of TeX and text-filled source for
   pasting into MS Wurd, and leave converted region on kill ring"
  (interactive)
  (let ((buf (get-buffer-create "*wordify-temp*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer))
    (save-excursion
      (copy-to-buffer buf (point) (mark))
      (set-buffer buf)
      (change-many '(("\n\n" "~@@~")
                     ("\n" " ")
                     ("~@@~" "\n")
                     ("``" "“")
                     ("''" "”")
                     ("\\\\emph{\\([^}]+\\)}" "/\\1/")
                     ("~?\\\\cite{\\([^}]+\\)}" " [\\1]")
                     ("\\%" "%")
                     ("--" "–")
                     ("---" "—")
                     ("\s-+" " ")))
      (copy-region-as-kill (point-min) (point-max)))))


(defun texify-region ()
  "Destructively convert region of pasted-in Wurd text to be TeX-friendly."
  (interactive)
  (save-excursion
    (narrow-to-region (point) (mark))
    (change-many '(("\\([^\\]\\)\\$" "\\1\\\\$")
                   ("^%" "~@@~") ("\\([^\\]\\)%" "\\1\\\\%") ("~@@~" "%")
                   ("’" "'")
                   ("‘" "`")
                   ("“" "``")
                   ("”" "''")
                   ("…" "\\\\ldots{}")
                   ("\\.\\.\\." "\\\\ldots{}")
                   ("\"\\([^\"]+\\)\"" "``\\1''")
                   ("—" "---")
                   ("–" "--")
                   ("½" "$1/2$")
                   ("¼" "$1/4$")
                   ))
    (fill-individual-paragraphs (point-min) (point-max))
    (widen)))


(defun empty-region (nlines)
  "Convert filled paragraphs to unfilled paragraphs in region. With prefix arg,
insert that many blank lines between paragraphs (default 0)."
  (interactive "p")
  (replace-all-in-region '(("\\\n\\\n+" "@@@@")
                           ("\\s-*\\\n\\s-*" " ")
                           ("@@@@" "\n"))))


(defun copy-region-as-empty ()
  "Convert region to empty paragraphs and place it on the kill ring without
deleting it."
  (interactive)
  (save-restriction
    (save-excursion
      (empty-region 0)
      (copy-region-as-kill (point) (mark))
      (undo))))

(defun word-outline-to-latex ()
  "Convert multilevel (numbered) Outline text pasted from Word into section,
  subsection, etc. structure of laTeX."
  (interactive)
  (replace-all-in-region '(("^\\s-*[0-9]+\\s-+\\(.*\\)\\s-*$"
                            "\\\\section{\\1}")
                           ("^\\s-*[0-9]+\\.[0-9]+\\s-+\\(.*\\)\\s-*$"
                            "\\\\subsection{\\1}")
                           ("^\\s-*[0-9]+\\.[0-9]+\\.[0-9]+\\s-+\\(.*\\)\\s-*$"
                            "\\\\subsubsection{\\1}")
                           ("^\\\\" "\n\\\\")
                           )))

(defun replace-all-in-region (lst)
  (save-restriction
    (narrow-to-region (point) (mark))
    (let ((case-replace-search nil))
      (dolist (pair lst)
        (goto-char (point-min))
        (while (re-search-forward (first pair) nil t)
          (replace-match (second pair) nil nil))))))
