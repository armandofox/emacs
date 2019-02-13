(require 'cl)
(adjoin (cons "\\.pl$" 'perl-mode) auto-mode-alist :test 'equal)
(adjoin (cons "\\.srm$" 'perl-mode) auto-mode-alist :test 'equal)

(defvar perl-file-regexp "^#!.*/.*perl\\b"
  "*If first line of buffer matches this regexp, it's assumed to be a Perl code
file.")

(defun find-perl-file-hook ()
  "Hook called to determine if a file contains Perl code by matching first line
against variable perl-file-regexp.  If perl, puts buffer in perl-mode."
  (if (save-excursion (bobp) (looking-at perl-file-regexp))
      (perl-mode)))

(push 'find-perl-file-hook find-file-hooks)

(defun insert-comment-quote (pfx)
  "Insert comment consisting of a single apostrophe to fix perl-mode
indentation for lines that refer to packagized variables.  With prefix,
insert double quote."
  (interactive "P")
  (indent-for-comment)
  (beginning-of-line)
  (let ((p (point)))
    (end-of-line)
    (search-backward "#" p)
    (forward-char 1)
    (insert (if pfx ?\" ?\'))))

(setq perl-mode-hook
      '(lambda ()
         (local-set-key "\C-m" 'newline-and-indent)
         (local-set-key "\M-'" 'insert-comment-quote)
         ;;(modify-syntax-entry ?\' "_" perl-mode-syntax-table)
         ;;(modify-syntax-entry ?\$ "\\" perl-mode-syntax-table)
         (auto-fill-mode 1)
         (setq comment-column 44)
         (setq perl-tab-to-comment nil)
         (setq perl-indent-level 4)
         (setq perl-continued-statement-offset 4)
         (setq perl-continued-brace-offset -4)
         (setq perl-brace-offset 0)
         (setq perl-brace-imaginary-offset 0)
         (setq perl-label-offset -2)
         (setq fill-column 79)
         (require 'perl-descr "perl-descr.el")
         (define-key perl-mode-map "\C-hf" 'describe-perl-symbol)))
