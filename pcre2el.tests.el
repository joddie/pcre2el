;;; pcre2el.tests.el -- tests for pcre2el/rxt

(require 'rxt)
(require 'ert)

;; Regexp quoting
(ert-deftest rxt-pcre-special-chars ()
  (let* ((string "String $ with (( ) regexp \\ special [a-z] characters")
         (re (pcre-to-elisp (concat "(\\Q" string "\\E)"))))
    (should (string-match re string))
    (should (equal (match-string 1 string) string))
    (should (equal (match-string 0 string) string))))

;; Grouping, alternation
(ert-deftest rxt-pcre-grouping ()
     (let ((re (pcre-to-elisp "(foo|bar)")))
       (should (string-match-p re "foo"))
       (should (string-match-p re "bar"))))

;; Grouping and character classes
(ert-deftest rxt-pcre-char-classes ()
  (let ((re (pcre-to-elisp "(\\D*):\\s*(\\d{3,5})$"))
        (string "Answer: 3501"))
    (should (string-match re string))
    (should (equal (match-string 1 string) "Answer"))
    (should (equal (match-string 2 string) "3501"))

    (should (not (string-match re "bad: 23")))
    (should (not (string-match re "also bad: 944732")))))

;;;; Weird rules for \digits
(ert-deftest rxt-pcre-digits ()
  ;; \040   is another way of writing a space
  (should (string-match-p (pcre-to-elisp "\040") " "))
  
  ;; \40    is the same, provided there are fewer than 40 previous capturing subpatterns
  (should (string-match-p (pcre-to-elisp "\40") " "))

  ;; \7     is always a back reference
  (let ((re
         (pcre-to-elisp
          "(.)(.)(.)(.)(.)(.)(.)\\s*\\7")))
    (should (string-match-p re "abcdefg g"))
    (should (not (string-match-p re "abcdefg\th"))))

  ;;\11    might be a back reference, or another way of writing a tab
  (should (string-match-p (pcre-to-elisp "\\11") "\t"))

  ;; Backreferences greater than 9 are unsupported in Emacs
  (should-error (pcre-to-elisp "(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)\\11"))

  ;; \011   is always a tab
  (should (string-match-p (pcre-to-elisp "\\011") "\t"))

  ;; \0113  is a tab followed by the character "3"
  (should (string-match-p (pcre-to-elisp "\\0113") "\t3"))

  ;; \113 might be a back reference, otherwise the character with octal
  ;; code 113
  (should (string-match-p (pcre-to-elisp "\\113") (char-to-string #o113)))

  ;;  \377 might be a back reference, otherwise the byte consisting
  ;;  entirely of 1 bits
  (should (string-match-p (pcre-to-elisp "\\377") (char-to-string 255)))

  ;; \81 is either a back reference, or a binary zero followed by the
  ;; two characters "8" and "1"
  (should (string-match-p (pcre-to-elisp "\\81") (concat (char-to-string 0) "81"))))

;; Character classes with special characters
(ert-deftest rxt-pcre-escapes-in-char-classes ()
  (let ((re (pcre-to-elisp "^[\\d\\w]*$")))
    (should (string-match-p re "012foo"))
    (should (not (string-match-p re "numbers 847 and 23 words"))))

  (let ((case-fold-search t))
    (should (string-match-p (pcre-to-elisp "^[\\dA-Z]*$")
                            "235711deadbeef"))))

;; Negated specials in character classes
(ert-deftest rxt-pcre-negated-char-class-escapes ()
  (let ((re (pcre-to-elisp "^[^\\d]*$")))
    (should (string-match-p re "words without numbers"))
    (should-not (string-match-p re "words 2 and 4 numbers 8"))))

;; Hexadecimal and octal escapes
(ert-deftest rxt-pcre-hex-octal ()
  (should (string-match-p (pcre-to-elisp "\\xab") (char-to-string #xab)))
  (should (string-match-p (pcre-to-elisp "[\\xab]") (char-to-string #xab)))
  (should (string-match-p (pcre-to-elisp "\\x{237}") (char-to-string #x237)))
  (should (string-match-p (pcre-to-elisp "[\\x{237}]") (char-to-string #x237)))
  (should (string-match-p (pcre-to-elisp "[\\177]") (char-to-string #o177)))
  (should (string-match-p (pcre-to-elisp "[\\7]") (char-to-string 7))))

;; Control characters
(ert-deftest rxt-pcre-control-chars ()
  (should (string-match-p (pcre-to-elisp "\\cx") (kbd "C-x")))
  (should (string-match-p (pcre-to-elisp "\\cC\\cn") (kbd "C-c C-n"))))

;; Double negation (intersection) in character classes
(ert-deftest rxt-pcre-char-set-intersection ()
  (should (string-match-p (pcre-to-elisp "^[^\\W]*$") "foo")))

;;; pcre2el.tests.el ends here
