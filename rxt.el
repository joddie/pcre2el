;; rxt.el -- PCRE <-> Elisp <-> rx/SRE regexp syntax converter

;; Copyright (C) 2012 Jonathan Oddie

;;
;; Author:			j.j.oddie at gmail.com
;; Created:			4 June 2012
;; Updated:			9 June 2012

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;; This file incorporates work covered by the following copyright and  
;; permission notice: 
;;
;; Copyright (c) 1993-2002 Richard Kelsey and Jonathan Rees Copyright
;; (c) 1994-2002 by Olin Shivers and Brian D. Carlstrom. Copyright (c)
;; 1999-2002 by Martin Gasbichler. Copyright (c) 2001-2002 by Michael
;; Sperber.  All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met: 1. Redistributions of source code must retain the above
;; copyright notice, this list of conditions and the following
;; disclaimer. 2. Redistributions in binary form must reproduce the
;; above copyright notice, this list of conditions and the following
;; disclaimer in the documentation and/or other materials provided
;; with the distribution. 3. The name of the authors may not be used
;; to endorse or promote products derived from this software without
;; specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



;;; Commentary:

;; This library provides support for translating regexp syntax back
;; and forth between Emacs regular expressions, a limited subset of
;; PCRE (Perl Compatible Regular Expressions), and the S-expression
;; based `rx' and SRE forms.  More specifically, it provides an
;; abstract data type (ADT) for representing regular expressions,
;; parsers from PCRE and Elisp string notation to the, and "unparsers"
;; from the ADT to PCRE, `rx', and SRE syntaxes. (Conversion back to
;; Elisp regexps is handled in two steps, first to `rx' syntax, and
;; then using `rx-to-string' from the `rx' library).
;;
;; The main functions of interest are `rxt-elisp->rx',
;; `rxt-elisp->sre', `rxt-pcre->rx', `rxt-pcre->sre',
;; `rxt-pcre->elisp', and `rxt-elisp->pcre'. Additionally, various
;; bits of the RE-Builder package are re-defined in a blatantly
;; non-modular manner to support (emulated) PCRE syntax and conversion
;; back and forth between PCRE, Elisp and rx syntax. (TODO: fix this.)
;;
;; This code is partially based on Olin Shivers' reference SRE
;; implementation in scsh: see scsh/re.scm, scsh/spencer.scm and
;; scsh/posixstr.scm. In particular, it steals the idea of an abstract
;; data type for regular expressions and the general structure of the
;; string regexp parser and unparser. The data types for character
;; sets are extended in order to support symbolic translation between
;; character set expressions without assuming a small (Latin1)
;; character set. The string parser is also extended to parse a bigger
;; variety of constructions, including POSIX character classes and
;; various Emacs and Perl regexp assertions. Otherwise, only the bare
;; minimum of SRE's abstract data type is implemented: in particular,
;; regexps do not count their submatches.
;;

;; BUGS:
;; - PCRE quoting \Q ... \E doesn't work with quantifiers
;; - doesn't respect non-greediness of *?, +? and ??, though they exist
;;   both in PCRE and Elisp (but not in SRE or Rx)
;; - presumably many others
;;
;; TODO:
;; - parse PCRE's /x syntax (with embedded spaces)
;; - PCRE \g{-n} ?
;; - many other things

(require 'cl)
(require 'rx)
(require 're-builder)


;;; Scanner macro
(eval-when-compile
  (defmacro rxt-token-case (&rest cases)
    "Consume a token at point and evaluate corresponding forms.

CASES is a list of `cond'-like clauses, (REGEXP FORMS
...). Considering CASES in order, if the text at point matches
REGEXP then moves point over the matched string and returns the
value of FORMS. Returns `nil' if none of the CASES matches."
    (declare (debug (&rest (sexp &rest form))))
    `(cond
      ,@(mapcar
	 (lambda (case)
	   (let ((token (car case))
		 (action (cdr case)))
	     (if (eq token t)
		 `(t ,@action)
	       `((looking-at ,token)
		 (goto-char (match-end 0))
		 ,@action))))
	 cases))))


;;;; Regexp ADT

;;; Strings
(defstruct
  (rxt-string
   (:constructor rxt-string (chars)))
  chars)

(defvar rxt-empty-string (rxt-string ""))
(defvar rxt-trivial rxt-empty-string)

(defun rxt-trivial-p (re)
  (and (rxt-string-p re)
       (equal (rxt-string-chars re) "")))

;;; Other primitives
(defstruct (rxt-primitive
	    (:constructor rxt-primitive (pcre rx &optional (sre rx))))
  pcre rx sre)

(defvar rxt-bos (rxt-primitive "\\A" 'bos))
(defvar rxt-eos (rxt-primitive "\\Z" 'eos))

(defvar rxt-bol (rxt-primitive "^" 'bol))
(defvar rxt-eol (rxt-primitive "$" 'eol))

(defvar rxt-any (rxt-primitive "." 'nonl))

(defvar rxt-word-boundary (rxt-primitive "\\b" 'word-boundary))
(defvar rxt-not-word-boundary (rxt-primitive "\\B" 'not-word-boundary))

(defvar rxt-wordchar (rxt-primitive "\\w" 'wordchar))
(defvar rxt-not-wordchar (rxt-primitive "\\W" 'not-wordchar))

(defvar rxt-symbol-start (rxt-primitive nil 'symbol-start))
(defvar rxt-symbol-end (rxt-primitive nil 'symbol-end))

(defvar rxt-bow (rxt-primitive nil 'bow))
(defvar rxt-eow (rxt-primitive nil 'eow))


;;; Sequence
(defstruct
  (rxt-seq
   (:constructor make-rxt-seq (elts)))
  elts)

;;; Slightly smart sequence constructor:
;;; - Flattens nested sequences
;;; - Drops trivial "" elements
;;; - Empty sequence => ""
;;; - Singleton sequence is reduced to its one element.
(defun rxt-seq (res)		    ; Flatten nested seqs & drop ""'s.
  (let ((res (rxt-seq-flatten res)))
    (if (consp res)
	(if (consp (cdr res))
	    (make-rxt-seq res)		; General case
	  (car res))			; Singleton sequence
      rxt-trivial)))			; Empty seq -- ""

(defun rxt-seq-flatten (res)
  (if (consp res)
      (let ((re (car res))
	    (tail (rxt-seq-flatten (cdr res))))
	(cond ((rxt-seq-p re)		; Flatten nested seqs
	       (append (rxt-seq-flatten (rxt-seq-elts re)) tail))
	      ((rxt-trivial-p re) tail)	; Drop trivial elts
	      ((and (rxt-string-p re)	; Flatten strings
		    (consp tail)
		    (rxt-string-p (car tail)))
	       (cons
		(rxt-string
		 (concat (rxt-string-chars re)
			 (rxt-string-chars (car tail))))
		(cdr tail)))
	      (t (cons re tail))))
    '()))

;;; Choice
(defstruct
  (rxt-choice
   (:constructor make-rxt-choice (elts)))
  elts)

;; The empty choice (always fails)
(defvar rxt-empty (make-rxt-choice nil))
(defun rxt-empty-p (re)
  (and (rxt-choice-p re)
       (null (rxt-choice-elts re))))
  
;; Constructor
(defun rxt-choice (res)
  (let ((res (rxt-choice-flatten res)))
    ;; If all elts are char-class re's, fold them together.
    (if (and (not (zerop (length res)))
	     (every #'rxt-char-set-p res))
	(let ((cset (car res)))
	  (dolist (re (cdr res))
	    (setq cset (rxt-char-set-adjoin! cset re)))
	  ;; TODO: turn one-character csets into strings
	  cset)

    (if (consp res)
	(if (consp (cdr res))
	    (make-rxt-choice res)	; General case
	  (car res))		; Singleton sequence
      rxt-empty))))

(defun rxt-choice-flatten (res)
  (if (consp res)
      (let* ((re (car res))
	     (tail (rxt-choice-flatten (cdr res))))
	(cond ((rxt-choice-p re)		; Flatten nested choices
	       (append (rxt-choice-flatten (rxt-choice-elts re)) tail))
	      ((rxt-empty-p re) tail)	; Drop empty re's.
	      (t (cons re tail))))
    '()))

;;; Repetition
(defstruct
  (rxt-repeat
   (:constructor rxt-repeat (from to body)))
  from to body)

;;; Submatch
(defstruct
  (rxt-submatch
   (:constructor rxt-submatch (body)))
  body)

;;; Backreference (not in SRE)
(defstruct
  (rxt-backref
   (:constructor rxt-backref (n)))
  n)

;;; Syntax classes (Emacs only)
(defstruct rxt-syntax-class
  symbol)

(defun rxt-syntax-class (symbol)
  (if (assoc symbol rx-syntax)
      (make-rxt-syntax-class :symbol symbol)
    (error "Invalid syntax class symbol %s" symbol)))


;;; Char sets
;; char set ::= <re-char-set> 
;;            | <re-char-set-negation>
;;            | <re-choice> ; where all rxt-choice-elts are char sets
;;            | <re-char-set-intersection>

;; an rxt-char-set represents the union of any number of characters,
;; character ranges, and POSIX character classes
(defstruct rxt-char-set			; (| cse ...) or (~ cse ...)
  chars					; list of single characters
  ranges				; list of ranges (from . to)
  classes)				; list of character classes

(defun rxt-char-set (item)
  "Construct an abstract regexp character set from ITEM.

ITEM may be a single character, a string or list of characters, a
range represented as a cons (FROM . TO), a symbol representing a
POSIX class, or an existing character set which is returned
unchanged."
  (cond
   ((rxt-cset-p item) item)

   ((integerp item)
    (make-rxt-char-set :chars (list item)))

   ((consp item)
    (if (consp (cdr item))
	(make-rxt-char-set :chars item)
      (make-rxt-char-set :ranges (list item))))

   ((stringp item)
    (make-rxt-char-set :chars (mapcar 'identity item)))

   ((symbolp item)
    (make-rxt-char-set :classes (list item)))

   (t
    (error "Attempt to make char-set out of %S" item))))

;; Destructive char-set union 
(defun rxt-char-set-adjoin! (cset item)
  "Destructively add the contents of ITEM to character set CSET.

CSET must be an rxt-char-set. ITEM may also be an rxt-char-set,
or it can be any of these shortcut representations: a character,
range, or a posix class symbol.

The union of CSET and ITEM is returned; CSET may be destructively
modified."
  (assert (rxt-char-set-p cset))

  (cond
   ((integerp item)			; character
    (push item (rxt-char-set-chars cset)))

   ((consp item)			; range (from . to)
    (push item (rxt-char-set-ranges cset)))

   ((symbolp item)			; posix character class
    (push item (rxt-char-set-classes cset)))

   ((rxt-char-set-p item)
      (dolist (type (list (rxt-char-set-chars item)
			  (rxt-char-set-ranges item)
			  (rxt-char-set-classes item)))
	(dolist (thing type)
	  (rxt-char-set-adjoin! cset thing))))

   (t
    (error "Can't adjoin non-rxt-char-set, character, range or symbol %S" item)))
  cset)

;;; Complement of a character set or syntax class
(defstruct rxt-char-set-negation
  elt)					; rxt-char-set, 

;; Negation 
(defun rxt-negate (cset)
  "Return the negation of character set CSET.

CSET may be an `rxt-char-set', an `rxt-syntax-class', or an
`rxt-char-set-negation'; or a shorthand form of an rxt-char-set."

  (cond ((or (rxt-char-set-p cset)
	     (rxt-syntax-class-p cset))
	 (make-rxt-char-set-negation :elt cset))

	((or (integerp cset) (consp cset) (symbolp cset))
	 (make-rxt-char-set-negation
	  :elt (rxt-char-set cset)))

	((rxt-char-set-negation-p cset)
	 (rxt-char-set-negation-elt cset))

	(t
	 (error "Can't negate non-char-set or syntax class %s" cset))))
  
;;; Intersections of char sets

;; These are difficult to represent in general, but can be constructed
;; in Perl using double negation; for example: [^\Wabc] means the set
;; complement of [abc] with respect to the universe of "word
;; characters": (& (~ (~ word)) (~ ("abc"))) == (& word (~ ("abc")))
;; == (- word ("abc"))

(defstruct rxt-intersection
  elts)

(defun rxt-intersection (charsets)
  (let ((elts '())
	(cmpl (make-rxt-char-set)))
    (dolist (cset (rxt-int-flatten charsets))
      (cond
       ((rxt-char-set-negation-p cset)
	;; Fold negated charsets together: ~A & ~B = ~(A|B)
	(setq cmpl (rxt-char-set-adjoin! cmpl (rxt-char-set-negation-elt cset))))
       
       ((rxt-char-set-p cset)
	(push cset elts))

       (t
	(error "Can't take intersection of non-character-set %s" cset))))
    (if (null elts)
	(rxt-negate cmpl)
      (push (rxt-negate cmpl) elts)
      (make-rxt-intersection :elts elts))))
	
;; Constructor helper: flatten nested intersections
(defun rxt-int-flatten (csets)
  (if (consp csets)
      (let ((cset (car csets))
	    (tail (rxt-int-flatten (cdr csets))))
	(if (rxt-intersection-p cset)
	    (append (rxt-int-flatten (rxt-intersection-elts cset)) tail)
	  (cons cset tail)))
    '()))
	  
;;; Higher-level character set combinations: intersection and union
;; Here a cset may be: rxt-char-set, rxt-char-set-negation,
;; rxt-intersection, or rxt-choice (where all elts are csets)
(defun rxt-cset-p (cset)
  (or (rxt-char-set-p cset)
      (rxt-char-set-negation-p cset)
      (rxt-intersection-p cset)
      (and (rxt-choice-p cset)
	   (every #'rxt-cset-p (rxt-choice-elts cset)))))

(defun rxt-cset-union (csets)
  (let ((union (make-rxt-char-set))) 
    (dolist (cset csets)
      (if (rxt-choice-p union)
	  (setq union (rxt-choice (list union cset)))
	;; else, union is a char-set
	(cond
	 ((rxt-char-set-p cset)
	  (setq union (rxt-char-set-adjoin! union cset)))

	 ((rxt-char-set-negation-p cset)
	  (setq union (rxt-choice (list union cset))))

	 ((rxt-intersection-p cset)
	  (setq union (rxt-choice (list union cset))))

	 ((rxt-choice-p cset)
	  (setq union (rxt-choice (list union cset))))

	 (t
	  (error "Non-cset %S in rxt-cset-union" cset)))))
    union))

(defun rxt-cset-intersection (csets)
  (rxt-intersection csets))



;;;; ADT unparsers to rx and sre notation

;;; ADT -> rx notation
(defun rxt-adt->rx (re)
  (cond
   ((rxt-primitive-p re)
    (rxt-primitive-rx re))

   ((rxt-string-p re) (rxt-string-chars re))

   ((rxt-seq-p re)
    (cons 'seq (mapcar #'rxt-adt->rx (rxt-seq-elts re))))

   ((rxt-choice-p re)
    (cons 'or (mapcar #'rxt-adt->rx (rxt-choice-elts re))))

   ((rxt-submatch-p re)
    (if (rxt-seq-p (rxt-submatch-body re))
	(cons 'submatch
	      (mapcar #'rxt-adt->rx (rxt-seq-elts (rxt-submatch-body re))))
      (list 'submatch (rxt-adt->rx (rxt-submatch-body re)))))

   ((rxt-backref-p re)
    (list 'backref (rxt-backref-n re)))

   ((rxt-syntax-class-p re)
    (list 'syntax (rxt-syntax-class-symbol re)))

   ((rxt-repeat-p re)
    (let ((from (rxt-repeat-from re))
	  (to (rxt-repeat-to re))
	  (body (rxt-adt->rx (rxt-repeat-body re))))
      (cond
       ((and (zerop from) (null to)) (list '* body))
       ((and (equal from 1) (null to)) (list '+ body))
       ((and (zerop from) (equal to 1)) (list 'optional body))
       ((null to) (list '>= from body))
       ((equal from to)
	(list '= from body))
       (t
	(list '** from to body)))))

   ((rxt-char-set-p re)
    (if (and (null (rxt-char-set-chars re))
	     (null (rxt-char-set-ranges re))
	     (= 1 (length (rxt-char-set-classes re))))
	(car (rxt-char-set-classes re))
      (append
       '(any)
       (and (rxt-char-set-chars re)
	    (list (mapconcat 'char-to-string (rxt-char-set-chars re) "")))

       (mapcar
	(lambda (range)
	  (format "%c-%c" (car range) (cdr range)))
	(rxt-char-set-ranges re))

       (rxt-char-set-classes re))))

   ((rxt-char-set-negation-p re)
    (list 'not (rxt-adt->rx (rxt-char-set-negation-elt re))))

   (t
    (error "No RX translation for %s" re))))

;;; ADT -> SRE notation
(defun rxt-adt->sre (re)
  (cond
   ((rxt-primitive-p re)
    (rxt-primitive-sre re))

   ((rxt-string-p re) (rxt-string-chars re))

   ((rxt-seq-p re)
    (cons ': (mapcar #'rxt-adt->sre (rxt-seq-elts re))))

   ((rxt-choice-p re)
    (cons '| (mapcar #'rxt-adt->sre (rxt-choice-elts re))))

   ((rxt-submatch-p re)
    (if (rxt-seq-p (rxt-submatch-body re))
	(cons 'submatch
	      (mapcar #'rxt-adt->sre (rxt-seq-elts (rxt-submatch-body re))))
      (list 'submatch (rxt-adt->sre (rxt-submatch-body re)))))

   ((rxt-repeat-p re)
    (let ((from (rxt-repeat-from re))
	  (to (rxt-repeat-to re))
	  (body (rxt-adt->sre (rxt-repeat-body re))))
      (cond
       ((and (zerop from) (null to)) (list '* body))
       ((and (equal from 1) (null to)) (list '+ body))
       ((and (zerop from) (equal to 1)) (list '\? body))
       ((null to) (list '>= from body))
       ((equal from to)
	(list '= from body))
       (t
	(list '** from to body)))))

   ((rxt-char-set-p re)
    (let* ((chars (mapconcat 'char-to-string (rxt-char-set-chars re) ""))

	   (ranges
	    (mapcar
	     (lambda (range)
	       (format "%c%c" (car range) (cdr range)))
	     (rxt-char-set-ranges re)))

	   (classes (rxt-char-set-classes re))

	   (all
	    (append
	     (if (not (zerop (length chars))) `((,chars)) nil)
	     (if ranges `((/ ,@ranges)) nil)
	     classes)))
      (if (> (length all) 1)
	  (cons '| all)
	(car all))))
      
   ((rxt-char-set-negation-p re)
    (list '~ (rxt-adt->sre (rxt-char-set-negation-elt re))))

   ((rxt-intersection re)
    (cons '& (mapcar #'rxt-adt->sre (rxt-intersection-elts re))))

   (t
    (error "No SRE translation for %s" re))))



;;;; ADT unparser to PCRE notation
;;; Based on scsh/posixstr.scm in scsh

(defun rxt-adt->pcre (re)
  (multiple-value-bind (s lev) (rxt-adt->pcre/lev re) s))

(defun rxt-adt->pcre/lev (re)
  (cond
   ((rxt-primitive-p re)
    (let ((s (rxt-primitive-pcre re)))
      (if s
	  (values s 1)
	(error "No PCRE translation for %s" re))))

   ((rxt-string-p re) (rxt-string->pcre re))
   ((rxt-seq-p re) (rxt-seq->pcre re))
   ((rxt-choice-p re) (rxt-choice->pcre re))

   ((rxt-submatch-p re) (rxt-submatch->pcre re))

   ((rxt-repeat-p re) (rxt-repeat->pcre re))

   ((or (rxt-char-set-p re)
	(rxt-char-set-negation-p re))
    (rxt-char-set->pcre re))

   ;; ((rxt-intersection re) (rxt-char-set-intersection->pcre re))

   (t
    (error "No PCRE translation for %s" re))))

(defvar rxt-pcre-metachars (rx (any "\\^.$|()[]*+?{}")))
(defvar rxt-pcre-charset-metachars (rx (any "]" "[" "\\" "^" "-")))

;; levels: 0 = parenthesized; 1 = piece; 2 = branch; 3 = top
(defun rxt-string->pcre (re)
  (values
   (replace-regexp-in-string
    rxt-pcre-metachars
    "\\\\\\&" (rxt-string-chars re))
   2))

(defun rxt-seq->pcre (re)
  (let ((elts (rxt-seq-elts re)))
    (if (null elts)
	""
      (rxt-seq-elts->pcre elts))))

(defun rxt-seq-elts->pcre (elts)
  (multiple-value-bind
      (s lev) (rxt-adt->pcre/lev (car elts))
    (if (null (cdr elts))
	(values s lev)
      (multiple-value-bind
	  (s1 lev1) (rxt-seq-elts->pcre (cdr elts))
	(values (concat (rxt-paren-if-necessary s lev)
			(rxt-paren-if-necessary s1 lev1))
		2)))))

(defun rxt-paren-if-necessary (s lev)
  (if (< lev 3)
      s
      (concat "(?:" s ")")))

(defun rxt-choice->pcre (re)
  (let ((elts (rxt-choice-elts re)))
    (if (null elts)
	nil
      (rxt-choice-elts->pcre elts))))
	
(defun rxt-choice-elts->pcre (elts)
  (multiple-value-bind
      (s lev) (rxt-adt->pcre/lev (car elts))
    (if (null (cdr elts))
	(values s lev)
      (multiple-value-bind
	  (s1 lev1) (rxt-choice-elts->pcre (cdr elts))
	(values (concat s "|" s1) 3)))))

(defun rxt-submatch->pcre (re)
  (multiple-value-bind
      (s lev) (rxt-adt->pcre/lev (rxt-submatch-body re))
    (values (concat "(" s ")") 0)))

(defun rxt-repeat->pcre (re)
  (let ((from (rxt-repeat-from re))
	(to (rxt-repeat-to re))
	(body (rxt-repeat-body re)))
    (multiple-value-bind
	(s lev) (rxt-adt->pcre/lev body)
      (cond
       ((and to (= from 1) (= to 1)) (values s lev))
       ((and to (= from 0) (= to 0)) (values "" 2))
       (t
	(when (> lev 1)			; parenthesize non-atoms
	  (setq s (concat "(?:" s ")")
		lev 0))
	(values (if to
		    (cond ((and (= from 0) (= to 1)) (concat s "?"))
			  ((= from to)
			   (concat s "{" (number-to-string to) "}"))
			  (t
			   (concat s "{" (number-to-string from)
					  "," (number-to-string to) "}")))
		  (cond ((= from 0) (concat s "*"))
			((= from 1) (concat s "+"))
			(t (concat s "{" (number-to-string from) ",}"))))
		1))))))
	
(defun rxt-char-set->pcre (re)
  (cond ((rxt-char-set-p re)
	 (values
	  (concat "[" (rxt-char-set->pcre/chars re) "]") 1))

	((rxt-char-set-negation-p re)
	 (let ((elt (rxt-char-set-negation-elt re)))
	   (if (rxt-char-set-p elt)
	       (values
		(concat "[^" (rxt-char-set->pcre/chars elt) "]") 1)
	     (error "No PCRE translation of %s" elt))))

	(t
	 (error "Non-char-set in rxt-char-set->pcre: %s"))))
	 
;; Fortunately, easier in PCRE than in POSIX!
(defun rxt-char-set->pcre/chars (re)
  (flet
      ((escape
	(char)
	(let ((s (char-to-string char)))
	  (cond ((string-match rxt-pcre-charset-metachars s)
		 (concat "\\" s))

		((and (not (string= s " "))
		      (string-match "[^[:graph:]]" s))
		 (format "\\x{%x}" char))

		(t s)))))

    (let ((chars (rxt-char-set-chars re))
	  (ranges (rxt-char-set-ranges re))
	  (classes (rxt-char-set-classes re)))

      (concat
       (mapconcat #'escape chars "")
       (mapconcat #'(lambda (rg)
		      (format "%s-%s"
			      (escape (car rg))
			      (escape (cdr rg))))
		  ranges "")
       (mapconcat #'(lambda (class)
		      (format "[:%s:]" class))
		  classes "")))))


	       
  


;;;; String regexp -> ADT parser

(defvar rxt-parse-pcre nil)

(defun rxt-parse-exp ()
  (let ((bar (regexp-quote (if rxt-parse-pcre "|" "\\|"))))
    (if (not (eobp))
	(let ((branches '()))
	  (catch 'done
	    (while t
	      (let ((branch (rxt-parse-branch)))
		(setq branches (cons branch branches))
		(if (looking-at bar)
		    (goto-char (match-end 0))
		  (throw 'done (rxt-choice (reverse branches))))))))
      (rxt-choice nil))))

(defun rxt-parse-branch ()
  (let ((stop
	 (regexp-opt
	  (if rxt-parse-pcre '("|" ")") '("\\|" "\\)"))))
	(pieces '()))
    (catch 'done
      (while t
	(if (not (eobp))
	    (let ((piece
		   (if (looking-at stop)
		       rxt-empty-string
		     (rxt-parse-piece))))
	      (setq pieces (cons piece pieces))
	      (if (looking-at stop)
		  (throw 'done (rxt-seq (reverse pieces)))))
	  (throw 'done (rxt-seq (reverse pieces))))))))

(defun rxt-parse-piece ()
  (let ((atom (rxt-parse-atom)))
    (if (eobp)
	atom
      (rxt-token-case
       ;; Should we try to do something better with non-greedy *?
       ;; etc.?
       ((rx (: "*" (opt "?"))) (rxt-repeat 0 nil atom))
       ((rx (: "+" (opt "?"))) (rxt-repeat 1 nil atom))
       ((rx (: "?" (opt "?"))) (rxt-repeat 0 1 atom))

       ((if rxt-parse-pcre "{" "\\\\{")
	(multiple-value-bind (from to)
	    (rxt-parse-braces)
	  (rxt-repeat from to atom)))

       (t atom)))))

(defun rxt-parse-atom ()
  (if (eobp)
      (error "Unexpected end of regular expression")
    (if rxt-parse-pcre
	(rxt-parse-atom/pcre)
      (rxt-parse-atom/el))))

(defun rxt-parse-atom/common ()
  (rxt-token-case
   ("\\^" rxt-bol)
   ("\\$" rxt-eol)
   ("\\." rxt-any)
   ("\\[" (rxt-parse-char-class))
   ("\\\\w" rxt-wordchar)
   ("\\\\W" rxt-not-wordchar)
   ("\\\\b" rxt-word-boundary)
   ("\\\\B" rxt-not-word-boundary)))

(defun rxt-parse-atom/el ()
  (or (rxt-parse-atom/common)
      (rxt-token-case
       ("\\\\(" (rxt-parse-subgroup "\\\\)")) ; Subgroup
   
       ("\\\\`" rxt-bos)
       ("\\\\'" rxt-eos)
       ("\\\\<" rxt-bow)
       ("\\\\>" rxt-eow)

       ;; Syntax categories
       ((rx (: "\\"
	       (submatch (any "Ss"))
	       (submatch (any "-.w_()'\"$\\/<>|!"))))
	(let ((negated (string= (match-string 1) "S"))
	      (re
	       (rxt-syntax-class
		(car (rassoc (string-to-char (match-string 2))
			     rx-syntax)))))
	  (if negated (rxt-negate re) re)))

       ("\\\\\\([1-9]\\)"			; Backreference
	(rxt-backref (string-to-number (match-string 1))))

       ;; Any other escaped character
       ("\\\\\\(.\\)" (rxt-string (match-string 1)))

       ;; Everything else
       (".\\|\n" (rxt-string (match-string 0))))))

(defvar rxt-subgroup-count nil)

(defun rxt-parse-atom/pcre ()
  (or (rxt-parse-atom/common)

      (let ((char (rxt-parse-escapes/pcre)))
	(and char
	     (rxt-string (char-to-string char))))

      (rxt-token-case
       ("(" (rxt-parse-subgroup ")")) ; Subgroup
       
       ("\\\\A" rxt-bos)
       ("\\\\Z" rxt-eos)

       ("\\\\Q"				; begin regexp quoting: FIXME
	(let ((begin (point)))
	  (search-forward "\\E" nil t)
	  (rxt-parse-atom/pcre)))
       
       ;; Various character classes
       ("\\\\d" (rxt-char-set 'digit))
       ("\\\\D" (rxt-negate 'digit))
       ("\\\\h" (rxt-char-set pcre-horizontal-whitespace-chars))
       ("\\\\H" (rxt-negate pcre-horizontal-whitespace-chars))
       ("\\\\s" (rxt-char-set 'space))
       ("\\\\S" (rxt-negate 'space))
       ("\\\\v" (rxt-char-set pcre-vertical-whitespace-chars))
       ("\\\\V" (rxt-negate pcre-vertical-whitespace-chars))

       ("\\\\\\([0-9]+\\)"		; backreference or octal char?
	(let* ((digits (match-string 1))
	       (dec (string-to-number digits)))
	  ;; from "man pcrepattern": If the number is less than 10, or if
	  ;; there have been at least that many previous capturing left
	  ;; parentheses in the expression, the entire sequence is taken
	  ;; as a back reference.
	  (if (or (< dec 10)
		  (>= rxt-subgroup-count dec))
	      (rxt-backref dec)
	
	    ;; from "man pcrepattern": if the decimal number is greater
	    ;; than 9 and there have not been that many capturing
	    ;; subpatterns, PCRE re-reads up to three octal digits
	    ;; following the backslash, and uses them to generate a data
	    ;; character. Any subsequent digits stand for themselves.
	    (goto-char (match-beginning 1))
	    (re-search-forward "[0-7]\\{0,3\\}")
	    (rxt-string (char-to-string (string-to-number (match-string 0) 8))))))

       ;; Any other escaped character
       ("\\\\\\(.\\)" (rxt-string (match-string 1)))

       ;; Everything else
       (".\\|\n" (rxt-string (match-string 0))))))

(defun rxt-parse-escapes/pcre ()
  "Consume a one-char PCRE escape at point and return its codepoint equivalent.

Handles only those character escapes which have the same meaning
in character classes as outside them."
  (rxt-token-case
   ("\\\\a" #x07)  ; bell
   ("\\\\c\\(.\\)"                  ; control character
    (- (string-to-char (upcase (match-string 1)))
       64))
   ("\\\\e" #x1b)  ; escape
   ("\\\\f" #x0c)  ; formfeed
   ("\\\\n" #x0a)  ; linefeed
   ("\\\\r" #x0d)  ; carriage return
   ("\\\\t" #x09)  ; tab
   
   ("\\\\x\\([A-Za-z0-9]\\{2\\}\\)"
    (string-to-number (match-string 1) 16))
   ("\\\\x{\\([A-Za-z0-9]*\\)}"
    (string-to-number (match-string 1) 16))))

(defun rxt-parse-subgroup (close)
  (incf rxt-subgroup-count)
  (let ((shy (looking-at "\\?:")))
    (when shy (forward-char 2))
    (let ((rx (rxt-parse-exp)))
      (if (not (looking-at close))
	  (error "Subexpression missing close paren")
	(goto-char (match-end 0))
	(if shy rx (rxt-submatch rx))))))

(defun rxt-parse-braces ()
  (rxt-token-case
   ((if rxt-parse-pcre
	"\\([0-9]*\\),\\([0-9]+\\)}"
      "\\([0-9]*\\),\\([0-9]+\\)\\\\}")
    (values (string-to-number (match-string 1))
	    (string-to-number (match-string 2))))
   ((if rxt-parse-pcre "\\([0-9]+\\),}" "\\([0-9]+\\),\\\\}")
    (values (string-to-number (match-string 1)) nil))
   ((if rxt-parse-pcre "\\([0-9]+\\)}" "\\([0-9]+\\)\\\\}")
    (let ((a (string-to-number (match-string 1))))
      (values a a)))
   (t
    (error "Bad brace expression"))))

;; Parse a character set range [...]		 
(defvar rxt-posix-classes
  (rx
   (: "[:"
      (submatch
       (or "alnum" "alpha" "ascii" "blank" "cntrl" "digit" "graph" "lower"
	   "print" "punct" "space" "upper" "word" "xdigit"))
      ":]")))

(defun rxt-parse-char-class ()
 (when (eobp)
   (error "Missing close right bracket in regexp"))

  (let* ((negated (rxt-token-case
		   ("\\^" t)
		   (t nil)))
	 (begin (point))
	 (result (if negated
		     (rxt-negate (make-rxt-char-set))
		   (make-rxt-char-set)))
	 (builder (if negated
		      #'rxt-cset-intersection
		    #'rxt-cset-union)))
    (catch 'done
      (while t
	(when (eobp)
	  (error "Missing close right bracket in regexp"))
	
	(if (and (looking-at "\\]")
		 (not (= (point) begin)))
	    (throw 'done result)
	  (let* ((piece (rxt-parse-char-class-piece))
		 (cset
		  (if negated
		      (rxt-negate (rxt-char-set piece))
		    (rxt-char-set piece))))
	    (setq result
		  (funcall builder (list result cset)))))))
    ;; Skip over closing "]"
    (forward-char)
    result))

;; Within a charset, parse a single character, a character range or a
;; posix class. Returns the character (i.e. an integer), a cons (from
;; . to), or a symbol denoting the posix class
(defun rxt-parse-char-class-piece ()
  (let ((atom (rxt-parse-char-class-atom)))
    (if (and (integerp atom)
	     (looking-at "-[^]]"))
	(let ((r-end (rxt-maybe-parse-range-end)))
	  (if r-end (cons atom r-end) atom))
      atom)))

;; Parse a single character or posix class within a charset
;;
;; Doesn't treat ] or - specially -- that's taken care of in other
;; functions.
(defun rxt-parse-char-class-atom ()
  (or (and rxt-parse-pcre
	   (rxt-parse-char-class-atom/pcre))
      
      (rxt-token-case
       (rxt-posix-classes (intern (match-string 1)))
   
       ("\\[:[a-z]*:\\]"
	(error "Unknown posix class %s" (match-string 0)))
   
       ("\\[\\([.=]\\)[a-z]*\\1\\]"
	(error "%s collation syntax not supported" (match-string 0)))

       (".\\|\n" (string-to-char (match-string 0))))))

;; Parse backslash escapes inside PCRE character classes
(defun rxt-parse-char-class-atom/pcre ()
  (or (rxt-parse-escapes/pcre)
      (rxt-token-case
       ;; Backslash + digits => octal char
       ("\\\\\\([0-7]\\{1,3\\}\\)"    
	(string-to-number (match-string 1) 8))

       ;; Various character classes.
       ("\\\\d" 'digit)
       ("\\\\D" (rxt-negate 'digit))

       ("\\\\h" (rxt-char-set pcre-horizontal-whitespace-chars))
       ("\\\\H" (rxt-negate pcre-horizontal-whitespace-chars))

       ("\\\\s" (rxt-char-set pcre-whitespace-chars))
       ("\\\\S" (rxt-negate pcre-whitespace-chars))

       ("\\\\v" (rxt-char-set pcre-vertical-whitespace-chars))
       ("\\\\V" (rxt-negate pcre-vertical-whitespace-chars))

       ("\\\\w" 'word)
       ("\\\\W" (rxt-negate 'word))

       ;; Ignore other escapes
       ("\\\\\\(.\\)" (string-to-char (match-string 1))))))


;; Parse a possible range tail. Called when point is before a dash "-"
;; not followed by "]". Might fail, since the thing after the "-"
;; could be a posix class rather than a character; in that case,
;; leaves point where it was and returns nil.
(defun rxt-maybe-parse-range-end ()
  (let (r-end pos)
    (save-excursion
      (forward-char)
      (setq r-end (rxt-parse-char-class-atom)
	    pos (point)))

      (if (integerp r-end)
	  ;; This is a range: move after it and return the ending character
	  (progn
	    (goto-char pos)
	    r-end)
	;; Not a range.
	nil)))


;;; Public interface
(defun rxt-parse-re (re &optional pcre type)
  (let ((rxt-parse-pcre pcre)
	(rxt-subgroup-count 0)
	(case-fold-search nil))
    (with-temp-buffer
      (insert re)
      (goto-char (point-min))
      (let ((parse (rxt-parse-exp)))
	(case type
	  ((sre) (rxt-adt->sre parse))
	  ((rx) (rxt-adt->rx parse))
	  (t parse))))))

(defun rxt-elisp->rx (el)
  (rxt-parse-re el nil 'rx))

(defun rxt-elisp->sre (el)
  (rxt-parse-re el nil 'sre))

(defun rxt-pcre->rx (pcre)
  (rxt-parse-re pcre t 'rx))

(defun rxt-pcre->sre (pcre)
  (rxt-parse-re pcre t 'sre))

(defun rxt-pcre->elisp (pcre)
  (rx-to-string (rxt-pcre->rx pcre) t))

(defun rxt-elisp->pcre (el)
  (rxt-adt->pcre (rxt-parse-re el nil)))

;;; testing purposes only
(defun rxt-test (re &optional pcre)
  (interactive "x")
  (insert (format ";; %S\n" re))
  (let ((rx (rxt-parse-re re pcre 'rx)))
    (insert (format ";; %S\n\n" (rx-to-string rx t)))
    (insert (format "%S\n\n\n" rx))))
  
(defun rxt-pcre-test (pcre)
  (interactive "s")
  (rxt-test pcre t))

;; (let ((rxt-parse-pcre t))
;;   (let ((rx (rxt-parse-re "(?:cat(?:aract|erpillar|))")))
;;     (message "%S\n%S" rx (rx-to-string rx))))


;; (with-temp-buffer
;;   (insert "\\x41-\\x50\\x{070}[:alnum:]foo]")
;;   (goto-char 0)
;;   (let* ((rxt-parse-pcre t)
;; 	 (result (rxt-parse-char-class))
;; 	 (chars (rxt-char-set-chars result))
;; 	 (ranges (rxt-char-set-ranges result))
;; 	 (classes (rxt-char-set-classes result))
;; 	 (negated (rxt-negated result)))
;;     (message "Negated: %s\nChars: %s\nRanges: %s\nClasses: %s"
;; 	     negated
;; 	     (mapconcat #'char-to-string chars "")
;; 	     (mapconcat
;; 	      (lambda (range)
;; 		(let ((begin (car range))
;; 		      (end (cdr range)))
;; 		  (format "%c-%c" begin end)))
;; 	      ranges ", ")
;; 	     (mapconcat #'symbol-name classes ", "))))


;;;; RE-Builder extensions from re-builder.el -- to be turned into advice
(defun reb-change-syntax (&optional syntax)
  "Change the syntax used by the RE Builder.
Optional argument SYNTAX must be specified if called non-interactively."
  (interactive
   (list (intern
	  (completing-read "Select syntax: "
			   (mapcar (lambda (el) (cons (symbol-name el) 1))
				   '(read string pcre lisp-re sregex rx))
			   nil t (symbol-name reb-re-syntax)))))

  (if (memq syntax '(read string pcre lisp-re sregex rx))
      (let ((buffer (get-buffer reb-buffer)))
	(setq reb-re-syntax syntax)
	(when buffer
          (with-current-buffer reb-target-buffer
	    (case syntax
	      ((rx)
	       (setq reb-regexp-src
		     (format "'%S"
			     (rxt-elisp->rx reb-regexp))))
	      ((pcre)
	       (setq reb-regexp-src (rxt-elisp->pcre reb-regexp)))))
	  (with-current-buffer buffer
            (reb-initialize-buffer))))
    (error "Invalid syntax: %s" syntax)))

(defun reb-read-regexp ()
  "Read current RE."
  (save-excursion
    (cond ((eq reb-re-syntax 'read)
	   (goto-char (point-min))
	   (read (current-buffer)))

	  ((eq reb-re-syntax 'string)
	   (goto-char (point-min))
	   (re-search-forward "\"")
	   (let ((beg (point)))
	     (goto-char (point-max))
	     (re-search-backward "\"")
	     (buffer-substring-no-properties beg (point))))

	  ((eq reb-re-syntax 'pcre)
	   (goto-char (point-min))
	   (skip-syntax-forward "-")
	   (let ((beg (point)))
	     (goto-char (point-max))
	     (skip-syntax-backward "-")
	     (buffer-substring-no-properties beg (point))))

	  ((or (reb-lisp-syntax-p) (eq reb-re-syntax 'pcre))
	   (buffer-string)))))

(defun reb-insert-regexp ()
  "Insert current RE."

  (let ((re (or (reb-target-binding reb-regexp)
		(reb-empty-regexp))))
  (cond ((eq reb-re-syntax 'read)
	 (print re (current-buffer)))
	((eq reb-re-syntax 'pcre)
	 (insert "\n"
		 (or (reb-target-binding reb-regexp-src)
		     (reb-empty-regexp))
		 "\n"))
	((eq reb-re-syntax 'string)
	 (insert "\n\"" re "\""))
	;; For the Lisp syntax we need the "source" of the regexp
	((reb-lisp-syntax-p)
	 (insert (or (reb-target-binding reb-regexp-src)
		     (reb-empty-regexp)))))))

(defun reb-cook-regexp (re)
  "Return RE after processing it according to `reb-re-syntax'."
  (cond ((eq reb-re-syntax 'lisp-re)
	 (when (fboundp 'lre-compile-string)
	   (lre-compile-string (eval (car (read-from-string re))))))
	((eq reb-re-syntax 'sregex)
	 (apply 'sregex (eval (car (read-from-string re)))))
	((eq reb-re-syntax 'rx)
	 (rx-to-string (eval (car (read-from-string re))) t))
	((eq reb-re-syntax 'pcre)
	 (rxt-pcre->elisp re))
	(t re)))

(defun reb-update-regexp ()
  "Update the regexp for the target buffer.
Return t if the (cooked) expression changed."
  (let* ((re-src (reb-read-regexp))
	 (re (reb-cook-regexp re-src)))
    (with-current-buffer reb-target-buffer
      (let ((oldre reb-regexp))
	(prog1
	    (not (string= oldre re))
	  (setq reb-regexp re)
	  ;; Only update the source re for the lisp formats
	  (when (or (reb-lisp-syntax-p) (eq reb-re-syntax 'pcre))
	    (setq reb-regexp-src re-src)))))))



(provide 'rxt)
