;;; ftnchek.el --- ftnchek support for fortran mode.
;;
;; Author: Judah Milgram <milgram@cgpp.com>

(defvar ftnchek-mode-version "0.9")
(defvar ftnchek-mode-date "12/16/2002")

;; Keywords: fortran syntax semantic
;; Current version at: http://www.glue.umd.edu/~milgram/ftnchekel.html
;;
;; Copyright 1998-2002 Judah Milgram
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc.,  59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; ==================================================================
;;
;;; FTNCHEK: Ftnchek is a fortran 77 syntax and  semantics checker
;;  by Dr. Robert Moniot, <moniot@fordham.edu>. Get it at
;;  http://www.dsm.fordham.edu/~ftnchek/
;;
;;======================================================================
;;   
;;  INSTALLATION:
;;
;;  Install ftnchek.el somewhere in your lisp load path. Maybe add
;;  lines in your ~/.emacs along the lines of:
;;
;;  (setq my-path (concat (getenv "HOME") "/local/share/emacs/site-lisp"
;;  (setq load-path (cons my-path load-path))
;;  (add-hook 'fortran-mode-hook (require 'ftnchek-mode "ftnchek"))
;;
;;  Byte-compile ftnchek.el, if you want.
;;
;; To Do:
;;
;; How do we handle case were comments precede first subroutine in
;;         library file? ftnchek-mode thinks it's an unnamed main.
;; Soup up regexps to tolerate embedded blanks.
;; Documentation! Info file, etc. (for  ftnchek too!)
;; Splash-blurb if ftnchek not found; message where to get
;; Make ftnchek-flags easier for user to customize (one for buffer,
;;         one for subprogram)
;; 
;; ====================================================================
;;
;; Acknowledgements:
;; Bruce Ravel, Jinwei Shen, Richard Stallman, and many others for advice,
;; suggestions and testing. 
;; Also: Michael D. Prange and Dave Love for fortran mode
;; And especially: Bob Moniot for ftnchek!
;;                 
;; ====================================================================
;;  History:
;;  v 0.9 12/16/02  update Bob Moniot contact info
;;                update acks
;;                fix message bug in ftnchek-check-subprogram
;;                clean up some comments
;;                improved ftnchek-error-first-line
;;                many setq's changed to defvar
;;                simplified ftnchek-current-subprogram
;;                consolidated dangling parentheses   :)
;;                miscellaneous cleanup to permit byte-compile w/o warnings
;;  v 0.8 12/10/02 oops, regexp-opt causes problems, switch to regexp-or
;;  v 0.7 12/4/02 Tested with emacs 21
;;                Menu-bar renamed "Ftnchek" and simplified
;;                much internal cleanup and re-write
;;                "next-error" in compile mode works much better now
;;                removed some functions that are now in Fortran-mode
;;                re-did menu with "easy-menu"
;;                pipe ftnchek through sed to make file name look right
;;  v 0.6 6/17/98 placed completion-ignore-case in a let
;;                defvar ftnchek-mode
;;                defun ftnchek-mode
;;  V 0.5 6/14/98 implemented "ftnchek-next-error"
;;                played with ftnchek-flags (array=2) 
;;  V 0.4 6/12/98 added require to "compile"
;;                got "fortran-goto-subprogram" working
;;  V 0.3 6/11/98 first public release

(require 'fortran)
(require 'compile)

(defvar ftnchek-maintainer "<milgram@cgpp.com>")
(defvar ftnchek-flags nil)
(defvar ftnchek-startup-message) ; maybe do this with "let"?

(defvar ftnchek-mode nil
  "Mode variable for ftnchek minor mode")
(make-variable-buffer-local 'ftnchek-mode)

(defcustom ftnchek-buffer-flags
  "-arguments -noextern -declare -library -noarray -portability -usage=no-com-var-uninitialized -include=."
  "Ftnchek options to use when checking an entire buffer")
(defcustom ftnchek-subprogram-flags
  "-arguments -noextern -declare -library -noarray -portability -usage=no-com-var-uninitialized -include=."
  "Ftnchek options to use when checking an individual subprogram")
(defcustom ftnchek-f77-flags
  nil
  "F77 strictness flags that get toggled in pull-down menu")

(defun ftnchek-mode(&optional arg)
  "Ftnchek minor mode."
  (interactive "P")
  (setq ftnchek-mode
	(if (null arg)
	    (not ftnchek-mode)
	  (> (prefix-numeric-value arg)  0))))
;   (if ftnchek-mode  ... etc.

(defun ftnchek-temp-file(s)
  "Generate a temp file with .f suffix"
  (concat
   (make-temp-name
    (expand-file-name s temporary-file-directory))
   ".f"))

(defun ftnchek-delete-lines-forward()
  "Delete all lines starting with current line"
  (save-excursion
    (let ((begin (point))
	  (end (point-max)))
      (delete-region begin end))))

(defun ftnchek-mask-lines-before-here()
  "Replace all lines preceding point with blank lines"
 (save-excursion
   (while (= (forward-line -1) 0)
     (beginning-of-line)
     (let ((beg (point)))
       (end-of-line)
       (delete-region beg (point))))))

(defvar
 ftnchek-error-regexp-alist
 (list
;; line 1 col 2 file foo.f
  (list ".*line \\([0-9]+\\)\\( col \\([0-9]+\\)\\)? file \\([^ ;$|:\n\t]+\\)" 4 1 3)
;; "foo.f", line 14 col 19:
  (list "\"\\([^\"]+\\)\", \\(near \\)?line \\([0-9]+\\)\\( col \\([0-9]+\\)\\)?" 1 3 5)))


(defun ftnchek-region(ftnchek-flags)
  "Run ftnchek on a region using compile()"
  ;; first, last are character positions. Convert to line positions.
  (let ((temp-file (ftnchek-temp-file "ftnchek" ))
	(first (point))
	(last (mark))	)
    (copy-region-as-kill (point-min) (point-max))
    (with-temp-file temp-file
      (yank)
      (goto-char last)
      (ftnchek-delete-lines-forward)
      (goto-char first)
      (ftnchek-mask-lines-before-here)
      )
    (compile-internal (ftnchek-command temp-file ftnchek-flags (buffer-name)) "No more errors" nil nil ftnchek-error-regexp-alist nil nil nil nil )))

(defun ftnchek-command(file-name &optional flags real-name)
  "Form the command to run ftnchek"
  ;; begin and end are line numbers, not char numbers.
  ;; Start by sending file-name to stdout, possibly
  (let ((rval "ftnchek "))
    (if (not (eq flags nil))
	(setq rval (concat rval flags " "))
      )
    (if (not (eq ftnchek-f77-flags nil))
	(setq rval (concat rval ftnchek-f77-flags " "))
      )
    (setq rval (concat rval "-quiet " file-name))
    (if (not (eq real-name nil))
	;; a bit dangerous -
	;; we assume this means file-name is a temp file
	;; maybe not always the case !
      (setq rval (concat rval " | sed 's|" file-name
			 "|" real-name "|g' && rm -f " file-name)))
    rval))

(defun ftnchek-buffer()
  "Run ftnchek on current buffer."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (ftnchek-region ftnchek-buffer-flags)
    (message "Checking entire buffer %s" (buffer-name))))

(defun ftnchek-subprogram()
  "Run ftnchek on suprogram the cursor is in. You can run
   fortran-what-subprogram  to find out what subprogram that is."
  (interactive)
  (save-excursion
;; Use this for older versions of fortran-mode:
;;    (mark-fortran-subprogram)
;; As of fortran mode v 21.2 or maybe even earlier:
    (mark-defun)
    (ftnchek-region ftnchek-subprogram-flags))
    (message "Checking %s" (ftnchek-current-subprogram)))


(defun ftnchek-strict-f77()
  "Toggle on strict Fortran 77 compliance checking"
  (interactive)
  (if (equal ftnchek-f77-flags "-f77")
      (setq ftnchek-f77-flags nil)
    (setq ftnchek-f77-flags "-f77")))

; I'm not sure I like these but nobody's complaining.
(define-key fortran-mode-map "\C-x`" 'ftnchek-next-error)
(define-key fortran-mode-map "\M-s" 'ftnchek-subprogram)
(define-key fortran-mode-map "\M-b" 'ftnchek-buffer)
(define-key fortran-mode-map "\M-p" 'ftnchek-previous-subprogram)
(define-key fortran-mode-map "\M-n" 'ftnchek-next-subprogram)
(define-key fortran-mode-map "\M-f" 'ftnchek-first-executable)
(define-key fortran-mode-map "\M-h" 'ftnchek-what-subprogram)



;; Menu
;; Fortran-mode does this, but is it important for us too?
;;(unless (boundp 'ftnchek-mode-menu)  
  (easy-menu-define
   ftnchek-mode-menu fortran-mode-map "Ftnchek menu"
   '("Ftnchek"
     ["Check buffer   " ftnchek-buffer t]
     ["Check subprogram   " ftnchek-subprogram t]
     ["Next error   " ftnchek-next-error t]
     ["Ftnchek version   " ftnchek-version-display t]
     ["Strict F77   " ftnchek-strict-f77 :style toggle
        :selected (equal ftnchek-f77-flags "-f77") ]
     "----"
     ;;; These items really belong in the fortran mode menu:
     ["What subprogram?   " ftnchek-what-subprogram t]
     ["First executable   " ftnchek-first-executable t]
     ["Prev subprogram   " ftnchek-previous-subprogram t]
     ["Next subprogram   " ftnchek-next-subprogram t]
     ))
 ;; )


; Startup message. Possibly useless.
(setq ftnchek-startup-message
      (concat "ftnchek.el "
	      " Version "
	      ftnchek-mode-version
	      " "
	      ftnchek-mode-date
	      " bugs to "
	      ftnchek-maintainer))
(message ftnchek-startup-message)
(sleep-for 0.5)

(defun ftnchek-version-display()
"Print the ftnchek version and patch level."
(interactive)
(message (concat (ftnchek-version) "; ftnchek.el v. " ftnchek-mode-version)))

;; This should probably be done with a pipe and sed.
(defun ftnchek-version()
  "Return ftnchek version as a string."
  (let (first last outbuf)
    (setq outbuf (get-buffer-create "*Ftnchek*"))
    (set-buffer outbuf)
    (goto-char (point-min))
    (setq first (point))
    (goto-char (point-max))
    (setq last (point))
    (if (> last first) (kill-region first last))
    (call-process "ftnchek" nil outbuf nil "-help")
    (set-buffer outbuf)
    (goto-char (point-min))
    (if (null (search-forward "FTNCHEK")) nil
      (beginning-of-line)
      (setq first (point))
      (end-of-line)
      (setq last (point))
      (buffer-substring first last))))

(defun ftnchek-error-first-line()
  "set first line of multiline ftnchek error message to top of window"
  (let (( here (point)))
    (beginning-of-line)
    (if (not (looking-at "^.*\\(Warning\\|Error\\)"))
	(re-search-backward "^.*\\(Warning\\|Error\\)" nil t)
      (forward-line -1)
      (if (not (looking-at "^ *\\^"))
	  (goto-char here)
	(forward-line -1)
	(if (not (looking-at "^ *[0-9]+"))
	    (forward-line 2))
	)))
  (recenter 0))

(defun ftnchek-next-error()
"ftnchek mode wrapper for next-error"
(interactive)
(next-error)
(other-window 1)
(ftnchek-error-first-line) 
(other-window -1))


;; I hope this is a good idea
(setq compilation-error-regexp-alist
      (append ftnchek-error-regexp-alist
	    compilation-error-regexp-alist))



(provide 'ftnchek-mode)


;;; ***********************************************************************
;
;     Extra navigation stuff - maybe this functionality will be added to
;     Fortran mode, in which case we can drop it from here.
;
;     Note ftnchek-mode's idea of where a program unit begins or ends may
;     not agree with fortran-mode.

;;    some useful regexps:

(defun identity(x) x)
(defun regexp-or(s &optional parens)
  "OR together a bunch of regexp's. Optional argument if t adds outer parens"
  (let ((rval (mapconcat 'identity s "\\|")))
    (if (eq parens nil)
	rval
      (concat "\\(" rval "\\)"))))

;; What about embedded spaces?

(defvar ftnchek-first-six-regexp "^[0-9 ][0-9 ][0-9 ][0-9 ][0-9 ] +")
(defvar ftnchek-blank-line-regexp "^[ \t]*$")      
(defvar ftnchek-continuation-line-regexp "^[0-9 ][0-9 ][0-9 ][0-9 ][0-9 ][^ ] *")
(defvar ftnchek-comment-regexp "^[Cc]" )
(defvar ftnchek-symbolic-name-regexp "\\([a-zA-Z][a-zA-Z0-9]*\\)")

(defvar ftnchek-type-regexp-list (list
				"integer"
				"real"
				"double *precision"
				"complex"
				"double *complex" ; not standard
				"logical"
				"\\(character\\( *\\* *[0-9]+\\)?\\)" ))

(defvar ftnchek-type-regexp (regexp-or ftnchek-type-regexp-list t))

(defvar ftnchek-subprogram-end-regexp (concat ftnchek-first-six-regexp "end *$"))

(defvar ftnchek-program-unit-begin-regexp
      (concat ftnchek-first-six-regexp
	      (regexp-or (list
			  (concat ftnchek-type-regexp "? *function")
			  "subroutine"
			  "program"
			  "block *data") t)
	      " *" ftnchek-symbolic-name-regexp "?" ))

;; See F77 standard, section 7. Note this regexp can't pick up
;; statement functions, which F77 also classes as non-executable.

; The items commented out are covered in ftnchek-type-regexp
(defvar ftnchek-non-executable-keyword-regexp-list
      (list
       "block *data"
       "character"
       "common"
       "complex"
       "data"
       "dimension"
       "function"
       "double *complex" ; not strict f77
       "double *precision"
       "entry"
       "equivalence"
       "external"
       "format"
       "implicit"
       "include"   ; not strict f77
       "integer"
       "intrinsic"
       "logical"
       "parameter"
       "program"
       "real"
       "save"
       "subroutine"))

(defvar ftnchek-non-executable-statement-regexp
      (concat ftnchek-first-six-regexp
	      (regexp-or ftnchek-non-executable-keyword-regexp-list t)))


(defun  ftnchek-program-unit-title()
  "Return descriptive string for program unit, or nil"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at ftnchek-program-unit-begin-regexp))
	nil
      ;; Guess how I got those match-field numbers.
      (let (( title  (match-string 1) )
	    ( name   (match-string 5) ))
	(if (not (eq name nil))
	    (concat title " " name)
	  name)))))


(defun ftnchek-end-of-subprogram()
  "Move point to first character of end statement (or EOF)."
  (let (( here (point) ))
    (if (re-search-forward ftnchek-subprogram-end-regexp nil 1 )
	(beginning-of-line)
      (if (re-search-backward ftnchek-subprogram-end-regexp nil t)
	  (beginning-of-line)
	(message "No end statement found beyond this point.")))
      (goto-char here)))

(defun ftnchek-find-program-unit-statement( N )
  "Move point either forwards or backwards to program unit start statement,
and return the title, or nil. N is either 1 (forward) or -1 (backward)"
  (interactive "p")
  (beginning-of-line)
  (let ((name nil))
    (while (and (not (setq name (ftnchek-program-unit-title)))
		(eq (forward-line N) 0)))
    (if (and (eq name nil)
	     (eq N -1))
	(setq name "unnamed main program")
      )
    name ))

(defun ftnchek-beginning-of-subprogram()
  "Move point to start of a program unit. Could be beginning of file.
   Returns title of program unit."
  (interactive)
  (beginning-of-line)
  ;; interstitial comments belong to following subprogram
  (ftnchek-end-of-subprogram)
  (ftnchek-find-program-unit-statement -1)
  )

(defun ftnchek-current-subprogram()
  "Return name of current subprogram without actually moving point"
  (save-excursion
    (ftnchek-beginning-of-subprogram)))
 
(defun ftnchek-what-subprogram()
  "Display the title of current Fortran subprogram"
  (interactive)
  (message (ftnchek-current-subprogram)))

(defun ftnchek-next-subprogram()
  "Move point to next subprogram"
  (interactive)
  (let (( here (point)) )
    (forward-line 1)
    (if (ftnchek-find-program-unit-statement 1)
	(message (ftnchek-current-subprogram))
      (message "Don't seem to be any more" )
      (goto-char here))))

(defun ftnchek-previous-subprogram()
  "Move point to previous subprogram"
  (interactive)
  ;; moving backwards, we always get a program unit name even if "unnamed main"
  (if (not (re-search-backward ftnchek-subprogram-end-regexp nil t))
	(message "Already seem to be in first one")
      (ftnchek-beginning-of-subprogram)
      (message (ftnchek-current-subprogram))))
  


(defun ftnchek-nonexecutable-statement()
  "t if current line is nonexecutable"
;; let's give it an optional arg so we can look at strings
  (or (looking-at ftnchek-non-executable-statement-regexp)
      (looking-at ftnchek-comment-regexp)
      (looking-at ftnchek-continuation-line-regexp)
      (looking-at ftnchek-blank-line-regexp)))

(defun ftnchek-next-executable-statement()
  "Skip to next executable statement"
  (while (ftnchek-nonexecutable-statement) (forward-line)))

(defun ftnchek-first-executable()
  "Move cursor to first executable statement in current subprogram"
  (interactive)
  (ftnchek-beginning-of-subprogram)
  (ftnchek-next-executable-statement)
  (message "First executable statement in %s" (ftnchek-current-subprogram)))
