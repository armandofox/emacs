;;; unmacify.el: Convert a Macintosh text document to ASCII
;;; Copyright (C) 1993 Wayne Mesard
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymous ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;--------------------------------------------------------------------

;;; EXAMPLE:
;;
;;  It turns this jumbled mass:            Into this stunning beauty:
;;  -------------------------------------  ------------------------------------
;;  This is the title^M^M  Here\325s the\  This is the title
;;   first paragraph.  It says what I wa\
;;  nt it to say and nothing more:^M\245\    Here's the first paragraph.  It
;;  \tHere's one thing that it says.^M\2\  says what I want it to say and
;;  45\tHere's a longer thing that it sa\  nothing more:
;;  ys.  It quotes someone else saying s\
;;  omething else, \322which is a common\    o Here's one thing that it
;;   thing in all kinds of technical and\      says.
;;   non-technical prose.\323  But then,\
;;   you already know that.^MFinally, th\    o Here's a longer thing that
;;  is document ends with a summary para\      it says.  It quotes someone
;;  graph explaining what a silly waste \      else saying something else,
;;  of time it really was to read it.          ``which is a common thing
;;                                             in all kinds of technical
;;                                             and non-technical prose.''
;;					       But then, you already know
;;					       that.
;;
;;					   Finally, this document ends with a
;;					   summary paragraph explaining what a
;;					   silly waste of time it really was to
;;					   read it.

;;; DESCRIPTION
;;   This is a not-too-complicated (the doc is twice as long as the code)
;;   little function that I wrote to create a readable ASCII version of
;;   documents created in MS Word on a Macintosh.  (Yeah, yeah, I know all
;;   about the boycott.  Look at it this way: this utility makes it easier
;;   for people to move documents off of Macs.)  It is by no means
;;   complete.  And it may not be useful for anyone except me.  In
;;   particular, if you use extra carriage returns to get whitespace
;;   between paragraphs (instead of specifying the space-after parameter in
;;   the Paragraph dialog box), you may have to tweak this to get it to do
;;   the right thing.
;;
;;   Usage:
;;    In Word, select Save As Text.  Do _not_ use Save As Text With Linebreaks.
;;    Upload the file.  Bring it into an Emacs buffer.  Then say 
;;    M-x unmacify-buffer.
;;
;;   What it does:
;;     o Replaces Returns with linefeeds.  This is only needed if you use a
;;       DOS disk to upload the file.
;;     o Replaces some of the Mac extended character set punctuation with
;;       their ASCII equivalents.  Currently, only single quote and open-
;;       and close-double quote are handled.
;;     o Scans for paragrpahs beginning with a "bullet" (option-8 on the
;;       Mac keyboard), and reformats them as you would a paragraph in an
;;       itemized list.
;;     o Fills lines that go past the buffer's fill-column.  In other
;;       words, things that look like titles and figures are left alone.
;;       Things that look like paragraphs are filled.
;;
;;   This is sufficient for my current needs.  If you think of something
;;   else you want it to do, or if you enhance it, please let me know.
;;
;;   Wayne Mesard: wmesard@cs.stanford.edu

;;; HISTORY
;;    1.0 wmesard - Mar 22, 1993: Created.


(defvar unmacify-alist
  '(("\^M" "\^J")
    ("\200" "AE")			; A-umlaut
    ("\201" "AA")			; A-circle
    ("\202" "C")			; C-cedilla
    ("\203" "E")			; E-accent-acute
    ("\204" "N")			; N-tilde
    ("\205" "OE")			; O-umlaut
    ("\206" "UE")			; U-umlaut
    ("\207" "a")			; a-accent-acute
    ("\210" "a")			; a-accent-grave
    ("\211" "a")			; a-circumflex
    ("\212" "ae")			; a-umlaut
    ("\213" "a")			; a-tilde
    ("\214" "aa")			; a-circle
    ("\215" "c")			; c-cedilla
    ("\216" "e")			; e-accent-acute
    ("\217" "e")			; e-accent-grave
    ("\220" "e")			; e-circumflex
    ("\221" "e")			; e-umlaut (dieresis)
    ("\222" "i")			; i-accent-acute
    ("\223" "i")			; i-accent-grave
    ("\224" "i")			; i-circumflex
    ("\225" "i")			; i-umlaut (dieresis)
    ("\226" "n")			; n-tilde
    ("\227" "o")			; o-accent-acute
    ("\230" "o")			; o-accent-grave
    ("\231" "o")			; o-circumflex
    ("\232" "oe")			; o-umlaut
    ("\233" "o")			; o-tilde
    ("\234" "u")			; u-accent-acute
    ("\235" "u")			; u-accent-grave
    ("\236" "u")			; u-circumflex
    ("\237" "ue")			; u-umlaut
    ("\240" "+")			; dagger
    ("\241C" "C")			; degrees C
    ("\241F" "F")			; degrees F
    ("\241" "F")			; degrees assume F
    ("\242" "c")			; cents
    ("\243" "#")			; pounds (should be L?)
    ("\244" "S")			; Section
    ("\245" "*")			; Bullet
    ("\246" "P")			; Paragraph
    ("\247" "ss")			; S-szet
    ("\250" "(R)")			; Registered trademark
    ("\251" "(C)")			; Copyright
    ("\252" "(tm)")			; Trademark
    ("\253" "'")			; Accent acute
    ("\254" ":")			; Umlaut
    ("\255" "!=")			; Not equal
    ("\256" "AE")			; Scandinavian AE
    ("\257" "OE")			; Scandinavian O-slash
    ("\260" "inf")			; Infinity
    ("\261" "+-")			; Plus-or-minus
    ("\262" "<=")			; Less-than-or-equal
    ("\263" ">=")			; Greater-than-or-equal
    ("\264" "Y")			; Yen
    ("\265" "u")			; Mu (micro)
    ("\266" "d")			; Curly-d
    ("\267" "Sum")			; Summation (Sigma)
    ("\270" "Prod")			; Product (Capital-PI)
    ("\271" "Pi")			; Pi
    ("\272" "Int")			; Integral
    ("\273" "a")			; Super-a (What is this thing?)
    ("\274" "o")			; Super-o (ditto?)
    ("\275" "Omega")			; Omega
    ("\276" "ae")			; Scandinavian ae
    ("\277" "oe")			; Scandinavian oe
    ("\300" "?")			; Upside-down ?
    ("\301" "!")			; Upside-down !
    ("\302" "!")			; Not
    ("\303" "Sqrt")			; Square root
    ("\304" "f")			; Script-f
    ("\305" "==")			; Approximately equal
    ("\306" "D")			; Delta
    ("\307" "<<")			; European open quote
    ("\310" ">>")			; European close quote 
    ("\311" "...")			; Ellipsis
    ("\312" " ")			; Non-breaking space
    ("\313" "A")			; A-accent-grave
    ("\314" "A")			; A-tilde
    ("\315" "O")			; O-tilde
    ("\316" "OE")			; OE
    ("\317" "oe")			; oe
    ("\320" "-")			; dash
    ("\321" "--")			; long-dash
    ("\322" "\"")			; open-double-quote
    ("\323" "\"")			; close-double-quote
    ("\324" "'")			; open-single-quote
    ("\325" "'")			; close-single-quote
    ("\326" "/")			; divide
    ("\327" "*")			; diamond
    ("\330" "y")			; y-umlaut (dieresis)
    ("\331" "Y")			; Y-umlaut (dieresis)
    ("\332" "/")			; fraction-slash
    ("\333" "$")			; currency
    ("\334" "<")			; European open-single-quote
    ("\335" ">")			; European close-single-quote
    ("\336" "fi")			; fi ligature
    ("\337" "fl")			; fl ligature
    ("\340" "+")			; double dagger
    ("\341" ".")			; centered dot
    ("\342" ",")			; lowered quote
    ("\343" ",,")			; lowered double quote
    ("\344" "%")			; per-thousand
    ("\345" "A")			; A-circumflex
    ("\346" "E")			; E-circumflex
    ("\347" "A")			; A-accent-acute
    ("\350" "E")			; E-umlaut (dieresis)
    ("\351" "E")			; E-accent-grave
    ("\352" "I")			; I-accent-acute
    ("\353" "I")			; I-circumflex
    ("\354" "I")			; I-umlaut (dieresis)
    ("\355" "I")			; I-accent-grave
    ("\356" "O")			; O-accent-acute
    ("\357" "O")			; O-circumflex
    ("\360" "Apple")			; The apple
    ("\361" "O")			; O-accent-grave
    ("\362" "U")			; U-accent-acute
    ("\363" "U")			; U-circumflex
    ("\364" "U")			; U-accent-grave
    ("\365" "1")			; small 1
    ("\366" "^")			; Circumflex
    ("\367" "~")			; Tilde
    ("\370" "-")			; Overbar
    ("\371" "-")			; cup (short-vowel symbol)
    ("\372" ".")			; raised dot
    ("\373" "o")			; raised circle
    ("\374" ",")			; cedilla
    ("\375" "\"")			; Inches
    ("\376" ",")			; Hook
    ("\377" "v")			; Hacek
    ("\^K" "\^J")			; MS-word table row separator
    )
  "Ascii approximations to special Macintosh characters")

(defvar unmacify-bullet "*")
(defvar unmacify-justify nil)

(defun unmacify-buffer ()
  (interactive)
  (unmacify-region (point-min) (point-max)))

(defun unmacify-region (beg end)
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (mapcar (function (lambda (x)
			(goto-char beg)
			(replace-string (car x) (cdr x))))
	    unmacify-alist)
    )
  (goto-char beg)
  (let ((fill-prefix "    ")
	;; The "6" below is "4+2" where 4 is the length of fill-prefix
	;; and 2 is the number of columns to reduce the right margin by.
	;; The "4" shouldn't be necessary, but its a workaround for a
	;; long-standing bug in fill.el
	(fill-column (- fill-column 6)))
    (while (re-search-forward "^\245\t" end t)
      (delete-backward-char 2)
      (insert "  " unmacify-bullet " ")
      (end-of-line nil)
      (fill-region (match-beginning 0) (point) unmacify-justify)
      ))
  (goto-char beg)
  (let ((long-line-regexp (concat "^" (make-string fill-column ?.))))
    (while (and (< (point) end) (re-search-forward long-line-regexp end t))
      (end-of-line nil)
      (fill-region (match-beginning 0) (point) unmacify-justify)
      ))
  )
