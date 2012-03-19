;;
;; pcre2el.el -- quick and dirty conversion from PCRE-style regexps to
;; Emacs Lisp syntax.
;;
;; Author:                   j.j.oddie at gmail.com
;; Hacked additiionally by:  opensource at hardakers dot net
;;  
;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.
;;
;; History:
;;   This was created out of an answer to a stackoverflow question:
;;      http://stackoverflow.com/questions/9118183/elisp-mechanism-for-converting-pcre-regexps-to-emacs-regexps
;;
;; Documentation:
;;   - Using the conversion function:
;;
;;     Use the following function to perform a conversion:
;;       (pcre-to-elisp "(abc|def)\\w+\\d+")
;;     Produces:
;;       "\(abc\|def\)\w+[0-9]+"
;;
;;   - Interactive replacements:
;;
;;     You may want to perform the following key bindings if you
;;     prefer PCRE generally over the elisp counterparts:
;;
;;     (global-set-key [(meta %)] 'pcre-query-replace-regexp)
;;
(eval-when-compile (require 'cl))

(defvar pcre-horizontal-whitespace-chars
  (mapconcat 'char-to-string
	     '(#x0009 #x0020 #x00A0 #x1680 #x180E #x2000 #x2001 #x2002 #x2003
		      #x2004 #x2005 #x2006 #x2007 #x2008 #x2009 #x200A #x202F
		      #x205F #x3000)
	     ""))

(defvar pcre-vertical-whitespace-chars
  (mapconcat 'char-to-string
	     '(#x000A #x000B #x000C #x000D #x0085 #x2028 #x2029) ""))

(defvar pcre-whitespace-chars
  (mapconcat 'char-to-string '(9 10 12 13 32) ""))

(defvar pcre-horizontal-whitespace
  (concat "[" pcre-horizontal-whitespace-chars "]"))

(defvar pcre-non-horizontal-whitespace
  (concat "[^" pcre-horizontal-whitespace-chars "]"))

(defvar pcre-vertical-whitespace
  (concat "[" pcre-vertical-whitespace-chars "]"))

(defvar pcre-non-vertical-whitespace
  (concat "[^" pcre-vertical-whitespace-chars "]"))

(defvar pcre-whitespace (concat "[" pcre-whitespace-chars "]"))

(defvar pcre-non-whitespace (concat "[^" pcre-whitespace-chars "]"))

(eval-when-compile
  (defmacro pcre-token-case (&rest cases)
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
	     `((looking-at ,token)
	       (goto-char (match-end 0))
	       ,@action)))
	 cases)
      (t nil))))

(defun pcre-query-replace-regexp (REGEXP TO-STRING &optional DELIMITED START END)
  "Use a PCRE regexp to search and replace with.
   This calls query-replace-regexp after converting the PCRE input to
   an elisp version of the search regexp"
  (interactive
   ;; the following interactive code was taken from replace.el from emacs
   (let ((common
          (query-replace-read-args
           (concat "Query replace"
                   (if current-prefix-arg " word" "")
                   " regexp"
                   (if (and transient-mark-mode mark-active) " in region" ""))
           t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
           ;; These are done separately here
           ;; so that command-history will record these expressions
           ;; rather than the values they had this time.
           (if (and transient-mark-mode mark-active)
               (region-beginning))
           (if (and transient-mark-mode mark-active)
               (region-end)))))
  (query-replace-regexp (pcre-to-elisp REGEXP) TO-STRING DELIMITED START END))

(defun pcre-to-elisp (pcre)
  "Convert PCRE, a regexp in PCRE notation, into Elisp string form."
  (with-temp-buffer
    (insert pcre)
    (goto-char (point-min))
    (let ((capture-count 0) (accum '())
	  (case-fold-search nil))
      (while (not (eobp))
	(let ((translated
	       (or
		;; Handle tokens that are treated the same in
		;; character classes
		(pcre-re-or-class-token-to-elisp)   

		;; Other tokens
		(pcre-token-case
		 ("|" "\\|")
		 ("(" (incf capture-count) "\\(")
		 (")" "\\)")
		 ("{" "\\{")
		 ("}" "\\}")

		 ;; Character class
		 ("\\[" (pcre-char-class-to-elisp))

		 ;; Backslash + digits => backreference or octal char?
		 ("\\\\\\([0-9]+\\)"
		  (let* ((digits (match-string 1))
			 (dec (string-to-number digits)))
		    ;; from "man pcrepattern": If the number is
		    ;; less than 10, or if there have been at
		    ;; least that many previous capturing left
		    ;; parentheses in the expression, the entire
		    ;; sequence is taken as a back reference.
		    (cond ((< dec 10) (concat "\\" digits))
			  ((>= capture-count dec)
			   (error "backreference \\%s can't be used in Emacs regexps"
				  digits))
			  (t
			   ;; from "man pcrepattern": if the
			   ;; decimal number is greater than 9 and
			   ;; there have not been that many
			   ;; capturing subpatterns, PCRE re-reads
			   ;; up to three octal digits following
			   ;; the backslash, and uses them to
			   ;; generate a data character. Any
			   ;; subsequent digits stand for
			   ;; themselves.
			   (goto-char (match-beginning 1))
			   (re-search-forward "[0-7]\\{0,3\\}")
			   (char-to-string (string-to-number (match-string 0) 8))))))

		 ;; Regexp quoting.
		 ("\\\\Q"
		  (let ((beginning (point)))
		    (search-forward "\\E")
		    (regexp-quote (buffer-substring beginning (match-beginning 0)))))

		 ;; Various character classes
		 ("\\\\d" "[0-9]")
		 ("\\\\D" "[^0-9]")
		 ("\\\\h" pcre-horizontal-whitespace)
		 ("\\\\H" pcre-non-horizontal-whitespace)
		 ("\\\\s" pcre-whitespace)
		 ("\\\\S" pcre-non-whitespace)
		 ("\\\\v" pcre-vertical-whitespace)
		 ("\\\\V" pcre-non-vertical-whitespace)

		 ;; Use Emacs' native notion of word characters
		 ("\\\\[Ww]" (match-string 0))

		 ;; Any other escaped character
		 ("\\\\\\(.\\)" (regexp-quote (match-string 1)))

		 ;; Any normal character
		 ("." (match-string 0))))))
	  (push translated accum)))
      (apply 'concat (reverse accum)))))

(defun pcre-re-or-class-token-to-elisp ()
  "Consume the PCRE token at point and return its Elisp equivalent.

Handles only tokens which have the same meaning in character
classes as outside them."
  (pcre-token-case
   ("\\\\a" (char-to-string #x07))  ; bell
   ("\\\\c\\(.\\)"                  ; control character
    (char-to-string
     (- (string-to-char (upcase (match-string 1))) 64)))
   ("\\\\e" (char-to-string #x1b))  ; escape
   ("\\\\f" (char-to-string #x0c))  ; formfeed
   ("\\\\n" (char-to-string #x0a))  ; linefeed
   ("\\\\r" (char-to-string #x0d))  ; carriage return
   ("\\\\t" (char-to-string #x09))  ; tab
   ("\\\\x\\([A-Za-z0-9]\\{2\\}\\)"
    (char-to-string (string-to-number (match-string 1) 16)))
   ("\\\\x{\\([A-Za-z0-9]*\\)}"
    (char-to-string (string-to-number (match-string 1) 16)))))

(defun pcre-char-class-to-elisp ()
  "Consume the remaining PCRE character class at point and return its Elisp equivalent.

Point should be after the opening \"[\" when this is called, and
will be just after the closing \"]\" when it returns."
  (let ((accum '("["))
	(alternatives '())
	(negated nil))
    (when (looking-at "\\^")
      (setq negated t)
      (push "^" accum)
      (forward-char))
    (when (looking-at "\\]") (push "]" accum) (forward-char))

    (while (not (looking-at "\\]"))
      (let ((translated
	     (or
	      (pcre-re-or-class-token-to-elisp)
	      (pcre-token-case              
	       ;; Backslash + digits => always an octal char
	       ("\\\\\\([0-7]\\{1,3\\}\\)"    
		(char-to-string (string-to-number (match-string 1) 8)))

	       ;; Various character classes. To implement negative char classes,
	       ;; we cons them onto the list `alternatives' and
	       ;; transform the char class into a shy group with alternation
	       ("\\\\d" "0-9")
	       ("\\\\D" (push (if negated "[0-9]" "[^0-9]")
			      alternatives) "")
	       ("\\\\h" pcre-horizontal-whitespace-chars)
	       ("\\\\H" (push (if negated
				  pcre-horizontal-whitespace
				pcre-non-horizontal-whitespace)
			      alternatives) "")
	       ("\\\\s" pcre-whitespace-chars)
	       ("\\\\S" (push (if negated
				  pcre-whitespace
				pcre-non-whitespace)
			      alternatives) "")
	       ("\\\\v" pcre-vertical-whitespace-chars)
	       ("\\\\V" (push (if negated
				  pcre-vertical-whitespace
				pcre-non-vertical-whitespace)
			      alternatives) "")
	       ("\\\\w" (push (if negated "\\W" "\\w") 
			      alternatives) "")
	       ("\\\\W" (push (if negated "\\w" "\\W") 
			      alternatives) "")

	       ;; Leave POSIX syntax unchanged
	       ("\\[:[a-z]*:\\]" (match-string 0))

	       ;; Ignore other escapes
	       ("\\\\\\(.\\)" (match-string 0))

	       ;; Copy everything else
	       ("." (match-string 0))))))
	(push translated accum)))
    (push "]" accum)
    (forward-char)
    (let ((class
	   (apply 'concat (reverse accum))))
      (when (or (equal class "[]")
		(equal class "[^]"))
	(setq class ""))
      (if (not alternatives)
	  class
	(concat "\\(?:"
		class "\\|"
		(mapconcat 'identity
			   alternatives
			   "\\|")
		"\\)")))))

(provide 'pcre2el)


;;;; A few simple tests

;; Regexp quoting
(let* ((string "String $ with (( ) regexp \\ special [a-z] characters")
       (re (pcre-to-elisp (concat "(\\Q" string "\\E)"))))
  (assert (string-match re string))
  (assert (equal (match-string 1 string) string))
  (assert (equal (match-string 0 string) string)))

;; Grouping, alternation
(let ((re (pcre-to-elisp "(foo|bar)")))
  (assert (string-match-p re "foo"))
  (assert (string-match-p re "bar")))

;; Grouping and character classes
(let ((re (pcre-to-elisp "(\\D*):\\s*(\\d{3,5})$"))
      (string "Answer: 3501"))
  (assert (string-match re string))
  (assert (equal (match-string 1 string) "Answer"))
  (assert (equal (match-string 2 string) "3501"))

  (assert (not (string-match re "bad: 23")))
  (assert (not (string-match re "also bad: 944732"))))

        ;;;; Weird rules for \digits
;; \040   is another way of writing a space
(assert (string-match-p (pcre-to-elisp "\040") " "))

;; \40    is the same, provided there are fewer than 40 previous capturing subpatterns
(assert (string-match-p (pcre-to-elisp "\40") " "))

;; \7     is always a back reference
(let ((re
       (pcre-to-elisp
	"(.)(.)(.)(.)(.)(.)(.)\\s*\\7")))
  (assert (string-match-p re "abcdefg g"))
  (assert (not (string-match-p re "abcdefg\th"))))

;;\11    might be a back reference, or another way of writing a tab
(assert
 (string-match-p (pcre-to-elisp "\\11") "\t"))
;; Backreferences greater than 9 don't work in Emacs regexps
(assert
 (condition-case nil
     (progn (pcre-to-elisp "(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)\\11") nil)
   (error t)))

;; \011   is always a tab
(assert
 (string-match-p
  (pcre-to-elisp "\\011") "\t"))

;; \0113  is a tab followed by the character "3"
(assert
 (string-match-p
  (pcre-to-elisp "\\0113") "\t3"))

;; \113 might be a back reference, otherwise the character with octal
;; code 113
(assert
 (string-match-p
  (pcre-to-elisp "\\113")
  (char-to-string #o113)))

;;  \377 might be a back reference, otherwise the byte consisting
;;  entirely of 1 bits
(assert
 (string-match-p
  (pcre-to-elisp "\\377")
  (char-to-string 255)))

;; \81 is either a back reference, or a binary zero followed by the
;; two characters "8" and "1"
(assert
 (string-match-p
  (pcre-to-elisp "\\81")
  (concat (char-to-string 0) "81")))


;; Character classes with special characters
(let ((re (pcre-to-elisp "^[\\d\\w]*$")))
  (assert (string-match-p re "012foo"))
  (assert (not (string-match-p re "numbers 847 and 23 words"))))

(assert
 (let ((case-fold-search t))
   (string-match-p
    (pcre-to-elisp "^[\\dA-Z]*$")
    "235711deadbeef")))

;; Negated specials in character classes
(let ((re (pcre-to-elisp "^[^\\d]*$")))
  (assert
   (string-match-p re "words without numbers"))
  (assert
   (not (string-match-p re "words 2 and 4 numbers 8"))))

;; Hexadecimal and octal escapes
(assert
 (string-match-p (pcre-to-elisp "\\xab") (char-to-string #xab))
 (string-match-p (pcre-to-elisp "[\\xab]") (char-to-string #xab))
 (string-match-p (pcre-to-elisp "\\x{237}") (char-to-string #x237))
 (string-match-p (pcre-to-elisp "[\\x{237}]") (char-to-string #x237))
 (string-match-p (pcre-to-elisp "[\\177]") (char-to-string #o177))
 (string-match-p (pcre-to-elisp "[\\7]") (char-to-string 7)))

;; Control characters
(assert
 (string-match-p (pcre-to-elisp "\\cx") (kbd "C-x"))
 (string-match-p (pcre-to-elisp "\\cC\\cn") (kbd "C-c C-n")))

;; Double negation in character classes (perverse)
(assert
 (string-match-p (pcre-to-elisp "^[^\\W]*$") "foo"))
