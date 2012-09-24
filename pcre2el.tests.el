;;; pcre2el.tests.el -- tests for pcre2el/rxt

(require 'pcre2el)

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

;;; pcre2el.tests.el ends here
