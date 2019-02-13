
;;; Generic completion functions

;;; This macro and its helpers extend completing-read
;;; by creating a system of functions and variables
;;; to do completing reads on various lists of things
;;; which can be grepped by shell commands.

;;; I provide examples for BibTeX, \cite keys,
;;; TeX/LaTeX \ref labels
;;; and MH aliases (I can never remember my "easy to rember"
;;; MH aliases.

;;; To use, load from your .emacs.
;;; Sane people will almost undoubtedly want to change my key bindings
;;; (really, all packages should separate functionality and key bindings).
;;; You probably will also want to make these definitions mode specific
;;; and/or buffer local -- I don't, because I prefer modelessness --
;;; by calling them in your hooks instead of inline.

;;; $Header: /home/CVS/fox/emacs/glew-completions.el,v 1.1.1.1 1996/10/09 01:12:31 fox Exp $
;;; aglew@uiuc.edu Tue Oct  9 17:45:38 1990



(defmacro make-completion-stuff (
			      XXcompletionXX
			      XXshellcommandXX
			      XXfilesXX
			      XXfilespromptXX
			      XXpromptXX
			      XXkeybindingXX
			      XXtmpbufXX
			      )
	  "
Macro defining a family of variables and functions for completion based on a list
of targets obtained by executing a shell command.

Produces:
    XXcompletionXX-files
    	List of files to scan.
    XXcompletionXX-shell-command
    	Command used to grep out targets.
    build-XXcompletionXX-alist
    	Manually build or replace the target expansion list when it
    	is changed externally (interim: should be automatic).
    complete-XXcompletionXX
    	Perform a completing read on the list of targets, and insert
    	the result into the local buffer.

Arguments are all strings (symbols, etc. created by macro concatenation):
    XXcompletionXX
    	A generic \"root name\" for the functions to be created.
    	Eg. BibTeX \cite key expansion sets this to \"BibTeX-key\"
    XXshellcommandXX
    	A format pattern for the shell command to be executed that
    	will produce a list of targets for expansion, one per line.
    	Usually contains a %s that the XXfilesXX variable will
    	be substituted into.
    XXfilesXX
    	Shell pattern (wildcard and/or list) that is substituted into
    	XXshellcommandXX as an argument. Broken out for ease of modification.
    	Eg. BibTeX \cite key expansion sets this to \"*.bib\".
    XXfilespromptXX
    	Prompt to use when asking user to change XXfilesXX.
    XXpromptXX
    	Prompt to use when asking user to complete a target.
    XXkeybindingXX
    	Keybinding that the complete-XXcompletionXX command will
    	be globally bound to.
    XXtmpbufXX
    	Temporary buffer to work in.
"

       (`(progn
	(defvar (, (intern(concat  XXcompletionXX "-files"))) (, XXfilesXX)
		(, (concat "*List of " XXfilespromptXX " for " XXcompletionXX "(string substituted in " XXcompletionXX "-shell-command)")))
	(defvar (, (intern(concat  XXcompletionXX "-shell-command"))) (, XXshellcommandXX)
		(, (concat "*Format of shell command used to grep " XXpromptXX " --- " XXcompletionXX "-files is substituted")))
	(defvar (, (intern(concat  XXcompletionXX "-alist"))) nil
		(, (concat "Alist of " XXpromptXX " used by complete-" XXcompletionXX)))
	(setq (, (intern(concat  XXcompletionXX "-files"))) (, XXfilesXX))
	(setq (, (intern(concat  XXcompletionXX "-shell-command"))) (, XXshellcommandXX))
	(setq (, (intern(concat  XXcompletionXX "-alist"))) nil)
	
	(defun (, (intern(concat  "validate-" XXcompletionXX "-alist"))) ()
	       (if (, (intern(concat  XXcompletionXX "-alist")))
		   (, (intern(concat  XXcompletionXX "-alist")))
		   ((, (intern(concat  "build-" XXcompletionXX "-alist"))))))
	
	(defun (, (intern(concat  "build-" XXcompletionXX "-alist"))) ()
	       (, (concat "*Build alist of " XXpromptXX " used by complete-" XXcompletionXX))
	       (interactive)
	       (setq (, (intern(concat  XXcompletionXX "-alist")))
		     ((, (intern(concat  "get-" XXcompletionXX "-alist")))
		      (setq (, (intern(concat  XXcompletionXX "-files")))
			    (read-from-minibuffer (concat (, XXfilespromptXX) ": ")
						  (, (intern(concat  XXcompletionXX "-files"))))))))
	
	(make-global-binding (, XXkeybindingXX) (quote (, (intern(concat  "complete-" XXcompletionXX)))))
	
	(defun (, (intern(concat  "complete-" XXcompletionXX))) ()
	       (, (concat "*Completing read of " XXpromptXX))
	       (interactive)
	       (insert (completing-read (concat (, XXpromptXX) ": ")
					((, (intern(concat  "validate-" XXcompletionXX "-alist"))))
					nil nil nil)))
	
	(defun (, (intern(concat  "get-" XXcompletionXX "-alist"))) (files)
	       (save-excursion
		(let ((cd default-directory))
		     (set-buffer (get-buffer-create (, XXtmpbufXX)))
		     (erase-buffer)
		     (setq default-directory cd)
		     (shell-command (format (, (intern(concat  XXcompletionXX "-shell-command"))) files) t)
		     (list-to-alist (list-of-lines-in-buffer (current-buffer))))))
	)))



(make-completion-stuff "BibTeX-key"
		       "grep '@.*{.*,' %s  | sed -e 's/.*{//' -e 's/,.*//'"
		       "*.bib"
		       "BibTeX databases"
		       "BibTeX keys"
		       "\^c\^b"
		       "*BibTeX-tmp*"
		       )
(make-completion-stuff "TeX-label"
		       "grep 'newlabel' %s  | sed -e 's/.*newlabel{//' -e 's/}.*//'"
		       "*.aux"
		       "TeX .aux files"
		       "TeX labels"
		       "\^c\^l"
		       "*TeX-tmp*"
		       )
(make-completion-stuff "mh-alias"
		       "grep ':' %s  | sed -e 's/:.*//'"
		       (expand-file-name (substitute-in-file-name "~/Mail/mh-aliases"))
		       "MH alias file(s)"
		       "MH alias"
		       "\^cm"
		       "*mh-tmp*"
		       )
(make-completion-stuff "mh-alias-expansion"
		       "grep ':' %s  | sed -e 's/.*:[ 	]*//'"
		       (expand-file-name (substitute-in-file-name "~/Mail/mh-aliases"))
		       "MH alias file(s)"
		       "MH alias-expansion"
		       "\^cM"
		       "*mh-tmp*"
		       )


;; INTERIM: this hack should go away when full tags facilities are available for iHDL

(make-completion-stuff "iHDL-expansion"
		       "/p6/lib/homes/glew/bin/iHDL-extract-names %s"
		       "*.hdl *.def *.ifc *.typ *.lst MainNet*"
		       "iHDL alias files"
		       "iHDL name"
		       "\^c\^h"
		       "*ihdl-tmp*"
		       )





;;;; Helper functions

(defun list-to-alist (list)
       "Convert a list to an alist, where each list element is car of each alist element"
       (mapcar 'list list))

(defun list-of-lines-in-buffer (buf)
       "Create a list from the lines in a buffer"
       (save-excursion
	(set-buffer buf)
	(let ((list nil))
	     (goto-char (point-min))
	     (while (not (eobp))
		    (re-search-forward ".*" (point-max) t)
		    (setq list (append list (list (buffer-substring (match-beginning 0) (match-end 0)))))
		    (re-search-forward "\n" (point-max) t))
	     list)))
