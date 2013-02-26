;;; pcre2el.el --- PCRE/Elisp/rx/SRE regexp syntax converter and utilities

;; Copyright (C) 2012 Jon Oddie <jonxfield@gmail.com>

;; Author:			joddie <jonxfield at gmail.com>
;; Hacked additionally by:	opensource at hardakers dot net
;; Created:			14 Feb 2012
;; Updated:			2 December 2012
;; Version:                     1.2
;; Url:                         https://github.com/joddie/pcre2el

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
;; Copyright (c) 1993-2002 Richard Kelsey and Jonathan Rees 
;; Copyright (c) 1994-2002 by Olin Shivers and Brian D. Carlstrom.
;; Copyright (c) 1999-2002 by Martin Gasbichler.
;; Copyright (c) 2001-2002 by Michael Sperber.
;; All rights reserved.
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
;;
;; 1.1 Overview 
;; =============
;;    This library provides Emacs support for a limited subset of
;;    Perl-compatible regular expression (PCRE) syntax by translating
;;    PCRE expresions into Emacs' native regexp syntax. It can also
;;    convert Emacs syntax to PCRE, convert both forms to S-expression
;;    based `rx' or SRE syntax, and do various other useful things.
;;
;;    PCRE has a complicated syntax and semantics, only some of which can
;;    be translated into Elisp. The PCRE subset which should be correctly
;;    parsed and converted is the following:
;;
;;   - parenthesis grouping `( .. )', including shy matches `(?: ... )'
;;   - backreferences (various syntaxes), but only up to 9 per expression    
;;   - alternation `|'
;;   - greedy and non-greedy quantifiers `*', `*?', `+', `+?', `?' and `??'
;;     (all of which are the same in Elisp as in PCRE)
;;   - numerical quantifiers `{M,N}'
;;   - beginning/end of string `\A', `\Z'
;;   - string quoting `\Q .. \E'
;;   - word boundaries `\b', `\B' (these are the same in Elisp)
;;   - single character escapes `\a', `\c', `\e', `\f', `\n', `\r', `\t',
;;     `\x', and `\octal digits' (but see BUGS, below, about non-ASCII
;;     characters)
;;   - character classes `[...]' including Posix escapes
;;   - character classes `\d', `\D', `\h', `\H', `\s', `\S', `\v', `\V'
;;     both within character class brackets and outside
;;   - word and non-word characters `\w' and `\W'
;;     (Emacs has the same syntax, but its meaning is different)
;;   - `s' (single line) and `x' (extended syntax) flags, in regexp
;;     literals, or set within the expression via `(?xs-xs)' or `(?xs-xs:
;;     .... )' syntax
;;   - comments `(?# ... )'
;;
;;
;; 1.2 Usage 
;; ==========
;;    Example of using the conversion functions:
;;    (rxt-pcre-to-elisp "(abc|def)\\w+\\d+")
;;       ;; => "\\(\\(?:abc\\|def\\)\\)[_[:alnum:]]+[[:digit:]]+"
;;
;;    You can also use `pcre-to-elisp' as an alias for `rxt-pcre-to-elisp'.
;;
;;    Besides `rxt-pcre-to-elisp', there are seven other conversion
;;    functions. They all take a single string argument, the regexp to
;;    translate:
;;
;;    - `rxt-elisp-to-pcre', `rxt-pcre-to-elisp'
;;    - `rxt-elisp-to-rx', `rxt-pcre-to-rx'
;;    - `rxt-elisp-to-sre', `rxt-pcre-to-sre'
;;    - `rxt-elisp-to-strings', `rxt-pcre-to-strings'
;;
;;    The two `-to-strings' functions are basically the inverse of
;;    `regexp-opt', producing a complete list of matching strings from a
;;    regexp. For obvious reasons, this only works with regexps that
;;    don't match an infinite set. Trying it with an expression that
;;    includes a `*', `+', or similar quantifier will throw an error.
;;
;;    (regexp-opt '("cat" "caterpillar" "catatonic"))
;;       ;; => "\\(?:cat\\(?:atonic\\|erpillar\\)?\\)"
;;    (rxt-elisp-to-strings "\\(?:cat\\(?:atonic\\|erpillar\\)?\\)")
;;        ;; => '("cat" "caterpillar" "catatonic")
;;
;; 1.2.1 Interactive usage 
;; ------------------------
;;     The translation functions can all be used interactively. You can
;;     choose your own key bindings for them. ;^) In interactive use they
;;     read the input regexp either from the current buffer or from the
;;     minibuffer as follows:
;;
;;     - If called with a prefix argument, both PCRE- and Elisp-expecting
;;       commands read a regexp from the minibuffer literally
;;       (i.e. without needing to double the backslashes). Without a
;;       prefix argument but when the region is active, they use the
;;       region contents, also literally.
;;     - With neither a prefix arg nor an active region, the commands
;;       that take an Emacs regexp as input behave like `C-x C-e': they
;;       use the result of evaluating the sexp before point (which could
;;       be a string literal). 
;;     - PCRE translators called without prefix arg or active region
;;       attempt to read a Perl-style delimited regexp literal
;;       *following* point in the current buffer, including its
;;       flags. For example, putting point before the `m' in the
;;       following example and doing `M-x pcre-to-elisp' displays
;;       `\(?:bar\|foo\)'
;;
;;       $x =~ m/  foo   |  (?# comment) bar /x
;;
;;       The PCRE reader currently only works with `/ ... /' delimiters. It
;;       will ignore any preceding `m', `s', or `qr' operator, as well as
;;       the replacement part of an `s' construction.
;;
;;     The translation functions display a result in the minibuffer and
;;     copy it to the kill ring. In the case of `rxt-pcre-to-elisp', you
;;     might need to use the result either literally, for input to an
;;     interactive command, or as a string to paste into Lisp code;
;;     therefore, this command copies both versions to the kill ring. You
;;     can get the literal regexp by doing `C-y' and the Lisp string by `C-y
;;     M-y'.
;;
;; 1.2.2 Query replace 
;; --------------------
;;     You may want to perform the following key bindings if you prefer
;;     PCRE generally over the Emacs counterparts:
;;
;;     (global-set-key [(meta %)] 'pcre-query-replace-regexp)
;;
;; 1.2.3 RE-Builder support 
;; -------------------------
;;     The Emacs RE-Builder comes with support for multiple syntaxes via
;;     `reb-change-syntax' (`C-c TAB'). It supports Elisp read and
;;     literal syntax, `rx' and `sregex' (another S-expression syntax),
;;     but only converts from the symbolic forms to Elisp, not the other
;;     way.  This package hacks the RE-Builder to also work with emulated
;;     PCRE syntax, and to convert transparently between Elisp, PCRE and
;;     rx syntaxes. PCRE mode reads a delimited literal `/ ... /', and
;;     should support using the `x' and `s' flags.
;;
;; 1.2.4 Explain regexps 
;; ----------------------
;;     Two commands, `rxt-explain-elisp' and `rxt-explain-pcre', can help
;;     untangle long regexps found in the wild. They read a regexp
;;     in the same way as the other interactive functions, convert it to
;;     `rx' S-expressions, and put the pretty-printed result in a new
;;     buffer. This is a work in progress.
;;
;; 1.3 Internal details 
;; =====================
;;    Internally, `pcre2el' defines a set of abstract data types for
;;    regular expressions, parsers from Elisp and PCRE syntax to the ADT,
;;    and unparsers from the ADT to PCRE, rx, and SRE syntax. Conversion
;;    from the ADT to Elisp syntax is a two-step process: first convert
;;    to `rx' form, then let `rx-to-string' do the heavy lifting. See
;;    `rxt-parse-re', `rxt-adt->pcre', `rxt-adt->rx', and `rxt-adt->sre',
;;    and the section beginning "Regexp ADT" in pcre2el.el for details.
;;
;;    This code is partially based on Olin Shivers' reference SRE
;;    implementation in scsh, although it is simplified in some respects
;;    and extended in others. See `scsh/re.scm', `scsh/spencer.scm' and
;;    `scsh/posixstr.scm' in the `scsh' source tree for details. In
;;    particular, `pcre2el' steals the idea of an abstract data type for
;;    regular expressions and the general structure of the string regexp
;;    parser and unparser. The data types for character sets are extended
;;    in order to support symbolic translation between character set
;;    expressions without assuming a small (Latin1) character set. The
;;    string parser is also extended to parse a bigger variety of
;;    constructions, including POSIX character classes and various Emacs
;;    and Perl regexp assertions. Otherwise, only the bare minimum of
;;    scsh's abstract data type is implemented.
;;
;; 1.4 Caveat 
;; ===========
;;    Having two incompatible regexp syntaxes in Lisp source code would
;;    be confusing for everyone. Please don't use this library to include
;;    PCRE syntax directly in Emacs packages that other people might use;
;;    instead, convert it to the usual Emacs Lisp syntax (or `rx' ;-)
;;
;;    Emacs regexps have their annoyances, but it is worth learning them
;;    properly. The Emacs assertions for word boundaries, symbol
;;    boundaries, and syntax classes are very useful, and don't have
;;    direct PCRE equivalents that I know of. Other things that might be
;;    done with huge regexps in other languages can be expressed more
;;    elegantly in Elisp using combinations of `save-excursion' with the
;;    various searches (regexp, literal, skip-syntax-forward,
;;    sexp-movement functions, etc.) </soapbox>
;;
;; 1.5 Bugs and Limitations 
;; =========================
;;    - Not namespace clean (`rxt-' and `pcre-'). Dunno which is better.
;;    - The order of alternatives and characters in char classes
;;      sometimes gets shifted around, which is annoying.
;;    - Although the string parser tries to interpret PCRE's octal and
;;      hexadecimal escapes correctly, there are problems with matching
;;      8-bit characters that I don't use enough to properly understand,
;;      e.g.:
;;      (string-match-p (rxt-pcre->elisp "\\377") "\377") => nil
;;      A fix for this would be welcome.
;;
;;    - Most of PCRE's rules for how `^', `\A', `$' and `\Z' interact with
;;      newlines in a string are not implemented; they don't seem as
;;      relevant to Emacs's buffer-oriented rather than
;;      string/line-oriented model.
;;
;;    - Many more esoteric PCRE features will never be supported because
;;      they can't be emulated by translation to Elisp regexps. These
;;      include the different lookaround assertions, conditionals, and
;;      the "backtracking control verbs" `(* ...)' . There are better ways
;;      to do those things in Elisp, anyway (IMHO ;^)
;;
;; 1.5.1 TODO : 
;; -------------
;;    - improve error reporting
;;    - PCRE `\L', `\U', `\l', `\u' case modifiers
;;    - PCRE `\g{...}' backreferences
;;    - PCREs in isearch mode
;;    - many other things
;;
;; 1.6 History 
;; ============
;;    This was originally created out of an answer to a stackoverflow
;;    question:
;;    [http://stackoverflow.com/questions/9118183/elisp-mechanism-for-converting-pcre-regexps-to-emacs-regexps]
;;    Thanks to Wes Hardaker for the initial inspiration and subsequent
;;    hacking, and to priyadarshan for requesting RX/SRE support!
;;

;;; Code:

(require 'cl)
(require 'rx)
(require 're-builder)

;;; Customization group
(defgroup rxt nil
  "Regex syntax converter and utilities."
  :version 1.2
  :group 'tools
  :group 'lisp
  :link '(emacs-commentary-link :tag "commentary" "pcre2el.el")
  :link '(emacs-library-link :tag "lisp file" "pcre2el.el")
  :link '(url-link :tag "web page" "https://github.com/joddie/pcre2el"))

(defface rxt-highlight
  '((t :inherit highlight))
  ""
  :group 'rxt)

(defcustom rxt-verbose-rx-translation nil
  "Non-nil if `rxt-pcre-to-rx' and `rxt-elisp-to-rx' should use verbose `rx' primitives.

Verbose primitives are things like `line-start' instead of `bol',
etc."
  :group 'rxt
  :type 'boolean)

(defcustom rxt-explain-verbosely t
  "Non-nil if `rxt-explain-elisp' and `rxt-explain-pcre' should use verbose `rx' primitives.

This overrides the value of `rxt-verbose-rxt-translation' for
these commands only."
  :group 'rxt
  :type 'boolean)


;;; User functions

;;;###autoload
(defun pcre-query-replace-regexp (regexp to-string &optional delimited start end)
  "Use a PCRE regexp to search and replace with.
   This calls query-replace-regexp after converting the PCRE input to
   an elisp version of the search regexp"
  (interactive
   ;; the following interactive code was taken from replace.el from emacs
   (let ((common
          (query-replace-read-args
           (concat "Query replace"
                   (if current-prefix-arg " word" "")
                   " (pcre)regexp"
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
  (query-replace-regexp (pcre-to-elisp regexp) to-string delimited start end))

;; Macro for returning values. If called interactively, display the
;; value in the echo area and copy it to the kill ring; otherwise just
;; return the value. TYPE controls what to copy to the kill ring. If
;; it is `pcre', copy the result as a string for yanking into Perl
;; code, JS code, etc. If `sexp', copy the result as a `read'-able
;; literal. If type is `emacs', copy both the string value (for use in
;; interactive commands) and a readable string literal (for yanking
;; into source buffers.
(eval-when-compile
  (defmacro rxt-value (type expr)
    (let ((val (make-symbol "val")))
      `(let ((,val ,expr))
         (if (called-interactively-p 'any)
             (let ((lisp-literal (format "%S" ,val)))
               (message "%s" ,val)
               ,(case type
                  (pcre
                   `(kill-new ,val))
                  (sexp
                   `(kill-new lisp-literal))
                  (emacs
                   `(progn
                      (kill-new lisp-literal)
                      (kill-new ,val)))
                  (t (error "Bad type: %s" type))))
           ,val)))))

;; Read an Elisp regexp interactively.
;;
;; Three possibilities: 
;;
;; 1) With a prefix arg, reads literally from the minibuffer, w/o
;; using string syntax -- just like query-replace-regexp, etc.
;;
;; 2) If the region is active, use the text of the region literally
;; (again w/o string syntax)
;;
;; 3) Otherwise, eval the sexp before point (which might be a string
;; literal or an expression) and use its value. Falls back to method
;; (1) if this fails to produce a string value.
;;
(defun rxt-interactive/elisp (&optional prompt)
  (list
   (let ((prompt (or prompt "Emacs regexp: ")))
     (cond (current-prefix-arg
            (read-string prompt))

           ((use-region-p)
            (buffer-substring-no-properties (region-beginning) (region-end)))
         
           (t
            (condition-case nil
                (let ((re (eval (preceding-sexp))))
                  (if (stringp re) re
                    (read-string prompt)))
              (error
               (read-string prompt))))))))

;; Read a PCRE regexp interactively.
;;
;; Three possibilities: As above, except that without prefix arg or
;; active region, tries to read a delimited regexp literal like /.../,
;; m/.../, or qr/.../ following point in the current buffer. Falls
;; back to reading from minibuffer if that fails.
;;
;; Returns two values: the regexp and the flags, if any.
;;
;; TODO: Different delimiters
;; 
(defun rxt-interactive/pcre (&optional prompt)
  (let ((prompt (or prompt "PCRE regexp: ")))
    (cond (current-prefix-arg
           (list (read-string prompt) ""))

          ((use-region-p)
           (list
            (buffer-substring-no-properties (region-beginning) (region-end))
            ""))

          (t
           (condition-case nil
               (rxt-read-delimited-pcre)
             (error
              (list (read-string prompt) "")))))))
                             
;; Translations from Emacs regexps to other formats
;;;###autoload
(defun rxt-elisp-to-pcre (regexp)
  "Translate REGEXP, a regexp in Emacs Lisp syntax, to Perl-compatible syntax.

Interactively, reads the regexp in one of three ways. With a
prefix arg, reads from minibuffer without string escaping, like
`query-replace-regexp'. Without a prefix arg, uses the text of
the region if it is active. Otherwise, uses the result of
evaluating the sexp before point (which might be a string regexp
literal or an expression that produces a string).

Displays the translated PCRE regexp in the echo area and copies
it to the kill ring.

Emacs regexp features such as syntax classes which cannot be
translated to PCRE will cause an error."
  (interactive (rxt-interactive/elisp))
  (rxt-value pcre (rxt-adt->pcre (rxt-parse-re regexp))))

;;;###autoload
(defun rxt-elisp-to-rx (regexp)
  "Translate REGEXP, a regexp in Emacs Lisp syntax, to `rx' syntax.

See `rxt-elisp-to-pcre' for a description of the interactive
behavior and `rx' for documentation of the S-expression based
regexp syntax."
  (interactive (rxt-interactive/elisp))
  (rxt-value sexp (rxt-adt->rx (rxt-parse-re regexp))))

;;;###autoload
(defun rxt-elisp-to-sre (regexp)
  "Translate REGEXP, a regexp in Emacs Lisp syntax, to SRE syntax.

See `rxt-elisp-to-pcre' for a description of the interactive behavior.

SRE is an S-expression notation for regular expressions developed
by Olin Shivers for scsh. See
http://www.scsh.net/docu/post/sre.html.

Emacs regexp features, including backreferences, which cannot be
translated to SRE will cause an error."
  (interactive (rxt-interactive/elisp))
  (rxt-value sexp (rxt-adt->sre (rxt-parse-re regexp))))

;;;###autoload
(defun rxt-elisp-to-strings (regexp)
  "Return a list of all strings matched by REGEXP, an Emacs Lisp regexp.

See `rxt-elisp-to-pcre' for a description of the interactive behavior.

This is useful primarily for getting back the original list of
strings from a regexp generated by `regexp-opt', but it will work
with any regexp without unbounded quantifiers (*, +, {2, } and so
on).

Throws an error if REGEXP contains any infinite quantifiers."
  (interactive (rxt-interactive/elisp))
  (rxt-value sexp (rxt-adt->strings (rxt-parse-re regexp))))

;; Translations from PCRE to other formats
;;;###autoload
(defun rxt-pcre-to-elisp (pcre &optional flags)
  "Translate PCRE, a regexp in Perl-compatible syntax, to Emacs Lisp.

Interactively, uses the contents of the region if it is active,
otherwise reads from the minibuffer. Prints the Emacs translation
in the echo area and copies it to the kill ring.

PCRE regexp features that cannot be translated into Emacs syntax
will cause an error. See the commentary section of pcre2el.el for
more details."
  (interactive (rxt-interactive/pcre))
  (rxt-value emacs (rx-to-string (rxt-pcre-to-rx pcre flags) t)))

;;;###autoload
(defalias 'pcre-to-elisp 'rxt-pcre-to-elisp)

;;;###autoload
(defun rxt-pcre-to-rx (pcre &optional flags)
  "Translate PCRE, a regexp in Perl-compatible syntax, to `rx' syntax.

See `rxt-pcre-to-elisp' for a description of the interactive behavior."
  (interactive (rxt-interactive/pcre))
  (rxt-value sexp (rxt-adt->rx (rxt-parse-re pcre t flags))))

;;;###autoload
(defun rxt-pcre-to-sre (pcre &optional flags)
  "Translate PCRE, a regexp in Perl-compatible syntax, to SRE syntax.

See `rxt-pcre-to-elisp' for a description of the interactive
behavior and `rxt-elisp-to-sre' for information about the SRE
S-expression format."
  (interactive (rxt-interactive/pcre))
  (rxt-value sexp (rxt-adt->sre (rxt-parse-re pcre t flags))))

;;;###autoload
(defun rxt-pcre-to-strings (pcre &optional flags)
  "Return a list of all strings matched by PCRE, a Perl-compatible regexp.

See `rxt-elisp-to-pcre' for a description of the interactive
behavior and `rxt-elisp-to-strings' for why this might be useful.

Throws an error if PCRE contains any infinite quantifiers."
  (interactive (rxt-interactive/pcre))
  (rxt-value sexp (rxt-adt->strings (rxt-parse-re pcre t flags))))

;;; "Explainers": display pretty-printed S-exp syntax for regexps 

;; When the `rxt-explain' flag is non-nil, `rxt-adt->rx' links each
;; part of the generated `rx' sexp to its corresponding part of the
;; parse tree structure, using `rxt-explain-hash-map'.  This allows
;; highlighting corresponding pieces of syntax at point.
(defvar rxt-explain nil)
(defvar rxt-explain-hash-map (make-hash-table :weakness 'key))
         
(defvar rxt-highlight-overlays nil
  "List of active location-highlighting overlays in rxt-help-mode buffer.")

;;;###autoload
(defun rxt-explain-elisp (regexp)
  "Insert the pretty-printed `rx' syntax for REGEXP in a new buffer.

REGEXP is a regular expression in Emacs Lisp syntax. See
`rxt-elisp-to-pcre' for a description of how REGEXP is read
interactively."
  (interactive (rxt-interactive/elisp))
  (let ((rxt-explain t)
        (rxt-verbose-rx-translation rxt-explain-verbosely))
    (rxt-pp-rx regexp (rxt-elisp-to-rx regexp))))

;;;###autoload
(defun rxt-explain-pcre (regexp &optional flags)
  "Insert the pretty-printed `rx' syntax for REGEXP in a new buffer.

REGEXP is a regular expression in PCRE syntax. See
`rxt-pcre-to-elisp' for a description of how REGEXP is read
interactively."
  (interactive (rxt-interactive/pcre))
  (let ((rxt-explain t)
        (rxt-verbose-rx-translation rxt-explain-verbosely))
    (rxt-pp-rx regexp (rxt-pcre-to-rx regexp flags))))

;; Generic major-mode-based dispatch
;; FIXME: This should be made into a minor mode w/ keybindings
(defun rxt-explain ()
  (interactive)
  (if (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
      (call-interactively #'rxt-explain-elisp)
    (call-interactively #'rxt-explain-pcre)))

;; Major mode for displaying pretty-printed S-exp syntax
(define-derived-mode rxt-help-mode emacs-lisp-mode "Regexp Explain"
  (setq buffer-read-only t)
  (add-hook 'post-command-hook 'rxt-highlight-text nil t)
  (rxt-highlight-text))

;; Hack: stop paredit-mode interfering with `rxt-print-rx'
(add-hook 'rxt-help-mode-hook
          (lambda ()
              (if paredit-mode (paredit-mode 0))))

(define-key rxt-help-mode-map "q" 'quit-window)
(define-key rxt-help-mode-map "z" 'kill-this-buffer)

(defun rxt-pp-rx (regexp rx)  
  "Display string regexp REGEXP with its `rx' form RX in an `rxt-help-mode' buffer."
  (with-current-buffer (get-buffer-create "* Regexp Explain *")
    (let ((print-escape-newlines t)
          (inhibit-read-only t))
      (erase-buffer)
      (rxt-help-mode) 
      (insert (format ";; %s\n\n" regexp))
      (save-excursion
        (let ((sexp-begin (point)))          
          (rxt-print-rx rx)
          (narrow-to-region sexp-begin (point))
          (pp-buffer)
          (widen)))
      (rxt-highlight-text))
    (pop-to-buffer (current-buffer))))

(defun* rxt-print-rx (rx &optional (depth 0))
  "Print RX like `print', adding text overlays for corresponding source locations."
  (let ((re (gethash rx rxt-explain-hash-map))
        (begin (point)))
    (if (consp rx)
        (progn 
          (insert "(")
          (loop for tail on rx
                do
                (let ((hd (car tail))
                      (tl (cdr tail)))
                  (rxt-print-rx hd (+ depth 1))
                  (if tl
                      (if (consp tl)
                          (insert " ")
                        (insert " . ")
                        (rxt-print-rx tl (+ depth 1))) 
                    (insert ")")))))
      (prin1 rx (current-buffer)))
    (when (and re
               (rxt-syntax-tree-begin re)
               (rxt-syntax-tree-end re))
      (let* ((sexp-begin (copy-marker begin t))
             (sexp-end (copy-marker (point)))
             (sexp-bounds (list sexp-begin sexp-end))
             (source-begin (+ 4 (rxt-syntax-tree-begin re)))
             (source-end (+ 4 (rxt-syntax-tree-end re)))
             (source-bounds (list source-begin source-end))
             (bounds (list source-bounds sexp-bounds))
             (sexp-ol (make-overlay sexp-begin sexp-end (current-buffer) t nil))
             (source-ol (make-overlay source-begin source-end (current-buffer) t nil)))
        (dolist (ol (list sexp-ol source-ol))
          (overlay-put ol 'priority depth)
          (overlay-put ol 'rxt-bounds bounds))))))

(defun rxt-highlight-text ()
  "Highlight the regex syntax at point and its corresponding RX/string form."
  (let ((all-bounds (get-char-property (point) 'rxt-bounds))
        (end (get-char-property (point) 'rxt-highlight-end)))
    (mapc #'delete-overlay rxt-highlight-overlays)
    (setq rxt-highlight-overlays nil)
    (dolist (bounds all-bounds)
      (destructuring-bind (begin end) bounds 
        (let ((overlay (make-overlay begin end)))
          (push overlay rxt-highlight-overlays)
          (overlay-put overlay 'face 'rxt-highlight))))))


;;; Constants
(defconst rxt-pcre-horizontal-whitespace-chars
  (mapconcat 'char-to-string
             '(#x0009 #x0020 #x00A0 #x1680 #x180E #x2000 #x2001 #x2002 #x2003
                      #x2004 #x2005 #x2006 #x2007 #x2008 #x2009 #x200A #x202F
                      #x205F #x3000)
             ""))

(defconst rxt-pcre-vertical-whitespace-chars
  (mapconcat 'char-to-string
	     '(#x000A #x000B #x000C #x000D #x0085 #x2028 #x2029) ""))

(defconst rxt-pcre-whitespace-chars
  (mapconcat 'char-to-string '(9 10 12 13 32) ""))


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

;; Base class that keeps the source text as a string with offsets
;; beginning and ending parsed portion
(defstruct
  rxt-syntax-tree
  begin end source)

;;; Strings
(defstruct
  (rxt-string
   (:constructor rxt-string (chars))
   (:include rxt-syntax-tree))
  chars)

(defvar rxt-empty-string (rxt-string ""))

(defun rxt-trivial-p (re)
  (and (rxt-string-p re)
       (equal (rxt-string-chars re) "")))

(defun rxt-string-concat (&rest strs)
  (if (null strs)
      rxt-empty-string
    (let ((result
           (rxt-string (reduce #'concat strs
                               :key #'rxt-string-chars))))
      (setf (rxt-syntax-tree-begin result) (rxt-syntax-tree-begin (first strs))
            (rxt-syntax-tree-end result) (rxt-syntax-tree-end (car (last strs))))
      result)))
;;; Other primitives
(defstruct (rxt-primitive
	    (:constructor rxt-primitive (pcre rx &optional (sre rx)))
            (:include rxt-syntax-tree))
  pcre rx sre)

(defvar rxt-bos (rxt-primitive "\\A" 'bos))
(defvar rxt-eos (rxt-primitive "\\Z" 'eos))

(defvar rxt-bol (rxt-primitive "^" 'bol))
(defvar rxt-eol (rxt-primitive "$" 'eol))

;; FIXME
(defvar rxt-anything (rxt-primitive "." 'anything))
(defvar rxt-nonl (rxt-primitive "." 'nonl))

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
   (:constructor make-rxt-seq (elts))
   (:include rxt-syntax-tree))
  elts)

;; Slightly smart sequence constructor:
;; - Flattens nested sequences
;; - Drops trivial "" elements
;; - Empty sequence => ""
;; - Singleton sequence is reduced to its one element.
(defun rxt-seq (res)		    ; Flatten nested seqs & drop ""'s.
  (let ((res (rxt-seq-flatten res)))
    (if (consp res)
	(if (consp (cdr res))
	    (make-rxt-seq res)		; General case
	  (car res))			; Singleton sequence
      rxt-empty-string)))               ; Empty seq -- ""

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
                (rxt-string-concat re (car tail))
		(cdr tail)))
	      (t (cons re tail))))
    '()))

;;; Choice
(defstruct
  (rxt-choice
   (:constructor make-rxt-choice (elts))
   (:include rxt-syntax-tree))
  elts)

;; The empty choice (always fails)
(defvar rxt-empty (make-rxt-choice nil))
(defun rxt-empty-p (re)
  (or
   (and (rxt-choice-p re)
        (null (rxt-choice-elts re)))
   (rxt-empty-char-set-p re)))
  
;; Slightly smart choice constructor:
;; -- flattens nested choices
;; -- drops never-matching (empty) REs
;; -- folds character sets and single-char strings together
;; -- singleton choice reduced to the element itself
(defun rxt-choice (res)
  (let ((res (rxt-choice-flatten res)))
    (if (consp res)
	(if (consp (cdr res))
	    (make-rxt-choice res)	; General case
	  (car res))                    ; Singleton choice
      rxt-empty)))

;; Flatten any nested rxt-choices amongst RES, and collect any
;; charsets together
(defun rxt-choice-flatten (res)
  (multiple-value-bind (res cset)
      (rxt-choice-flatten+char-set res)
    (if (not (rxt-empty-p cset))
        (cons cset res)
      res)))

;; Does the real work for the above. Returns two values: a flat list
;; of elements (with any rxt-choices reduced to their contents), and a
;; char-set union that collects together all the charsets and
;; single-char strings
(defun rxt-choice-flatten+char-set (res)
  (if (null res)
      (values '() (make-rxt-char-set-union))
    (let* ((re (car res)))
      (multiple-value-bind (tail cset)
          (rxt-choice-flatten+char-set (cdr res))
	(cond ((rxt-choice-p re)        ; Flatten nested choices
	       (values
                (append (rxt-choice-elts re) tail)
                cset))

	      ((rxt-empty-p re)         ; Drop empty re's.
               (values tail cset))

              ((rxt-char-set-union-p re) ; Fold char sets together
               (values tail
                       (rxt-char-set-adjoin! cset re)))

              ((and (rxt-string-p re)   ; Same for 1-char strings
                    (= 1 (length (rxt-string-chars re))))
               (values tail
                       (rxt-char-set-adjoin! cset
                                             (rxt-string-chars re))))

	      (t                        ; Otherwise.
               (values (cons re tail) cset)))))))

;;; Repetition
(defstruct (rxt-repeat
            (:include rxt-syntax-tree))
  from to body greedy)

(defun* rxt-repeat (from to body &optional (greedy t))
  (if (equal to 0)
      rxt-empty-string
    (make-rxt-repeat :from from :to to
                     :body body :greedy greedy)))

;;; Submatch
(defstruct
  (rxt-submatch
   (:constructor rxt-submatch (body))
   (:include rxt-syntax-tree))
  body)

;;; Backreference (not in SRE)
(defstruct
  (rxt-backref
   (:constructor rxt-backref (n))
   (:include rxt-syntax-tree))
  n)

;;; Syntax classes (Emacs only)
(defstruct (rxt-syntax-class
            (:include rxt-syntax-tree))
  symbol)

(defun rxt-syntax-class (symbol)
  (if (assoc symbol rx-syntax)
      (make-rxt-syntax-class :symbol symbol)
    (error "Invalid syntax class symbol %s" symbol)))

;;; Character categories (Emacs only)
(defstruct (rxt-char-category
            (:include rxt-syntax-tree))
  symbol)

(defun rxt-char-category (symbol)
  (if (assoc symbol rx-categories)
      (make-rxt-char-category :symbol symbol)
    (error "Invalid character category symbol %s" symbol)))


;;; Char sets
;; <rxt-char-set> ::= <rxt-char-set-union> 
;;                  | <rxt-char-set-negation>
;;                  | <rxt-choice> ; where all rxt-choice-elts are char sets
;;                  | <rxt-char-set-intersection>

(defun rxt-char-set-p (cset)
  (or (rxt-char-set-union-p cset)
      (rxt-char-set-negation-p cset)
      (rxt-char-set-intersection-p cset)
      ;; (and (rxt-choice-p cset)
      ;;      (every #'rxt-char-set-p (rxt-choice-elts cset)))
      ))

;; An rxt-char-set-union represents the union of any number of
;; characters, character ranges, and POSIX character classes: anything
;; that can be represented in string notation as a class [ ... ]
;; without the negation operator.
(defstruct (rxt-char-set-union
            (:include rxt-syntax-tree))
  chars					; list of single characters
  ranges				; list of ranges (from . to)
  classes)				; list of character classes

;; Test for empty character set
(defun rxt-empty-char-set-p (cset)
  (and (rxt-char-set-union-p cset)
       (null (rxt-char-set-union-chars cset))
       (null (rxt-char-set-union-ranges cset))
       (null (rxt-char-set-union-classes cset))))

;; Simple union constructor
(defun rxt-simple-char-set (item)
  "Construct an abstract regexp character set from ITEM.

ITEM may be a single character, a string or list of characters, a
range represented as a cons (FROM . TO), a symbol representing a
POSIX class, or an existing character set, which is returned
unchanged."
  (cond
   ((rxt-char-set-p item) item)

   ((integerp item)
    (make-rxt-char-set-union :chars (list item)))

   ((consp item)
    (if (consp (cdr item))              
	(make-rxt-char-set-union :chars item)         ; list of chars
      (make-rxt-char-set-union :ranges (list item)))) ; range (from . to)

   ((stringp item)
    (make-rxt-char-set-union :chars (mapcar 'identity item)))

   ((symbolp item)
    (make-rxt-char-set-union :classes (list item)))

   (t
    (error "Can't construct character set union from %S" item))))
	  
;;; Generalized union constructor: falls back to rxt-choice if
;;; necessary
(defun rxt-char-set-union (csets)
  (let ((union (make-rxt-char-set-union))) 
    (dolist (cset csets)
      (if (and (rxt-char-set-union-p cset)
               (rxt-char-set-union-p union))
          (setq union (rxt-char-set-adjoin! union cset))
        (setq union (rxt-choice (list union cset)))))
    union))

;; Destructive character set union
(defun rxt-char-set-adjoin! (cset item)
  "Destructively add the contents of ITEM to character set union CSET.

CSET must be an rxt-char-set. ITEM may be an rxt-char-set-union, or any
of these shortcut representations: a single character which
stands for itself, a cons (FROM . TO) representing a range, or a
symbol naming a posix class: 'digit, 'alnum, etc.

Returns the union of CSET and ITEM; CSET may be destructively
modified."
  (assert (rxt-char-set-union-p cset))

  (cond
   ((integerp item)			; character
    (push item (rxt-char-set-union-chars cset)))

   ((consp item)
    (if (consp (cdr item))
        (dolist (char item)             ; list of chars
          (rxt-char-set-adjoin! cset char))
      (push item (rxt-char-set-union-ranges cset)))) ; range (from . to)

   ((stringp item) 
    (mapc (lambda (char)
            (rxt-char-set-adjoin! cset char))
          item))

   ((symbolp item)			; posix character class
    (push item (rxt-char-set-union-classes cset)))

   ((rxt-char-set-union-p item)
    (dolist (type (list (rxt-char-set-union-chars item)
                        (rxt-char-set-union-ranges item)
                        (rxt-char-set-union-classes item)))
      (dolist (thing type)
        (rxt-char-set-adjoin! cset thing))))

   (t
    (error "Can't adjoin non-rxt-char-set, character, range or symbol %S" item)))
  cset)


;;; Set complement of character set, syntax class, or character
;;; category

;; In general, all character sets that can be represented in string
;; notation as [^ ... ] (but see `rxt-char-set-intersection', below), plus
;; Emacs' \Sx and \Cx constructions. 
(defstruct (rxt-char-set-negation
            (:include rxt-syntax-tree))
  elt)

;; Complement constructor: checks types, unwraps existing negations
(defun rxt-negate (cset)
  "Return the logical complement (negation) of CSET.

CSET may be one of the following types: `rxt-char-set-union',
`rxt-syntax-class', `rxt-char-category', `rxt-char-set-negation';
or a shorthand char-set specifier (see `rxt-char-set')`."
  (cond ((or (rxt-char-set-union-p cset)
	     (rxt-syntax-class-p cset)
             (rxt-char-category-p cset))
	 (make-rxt-char-set-negation :elt cset))

	((or (integerp cset) (consp cset) (symbolp cset) (stringp cset))
	 (make-rxt-char-set-negation
	  :elt (rxt-simple-char-set cset)))

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

(defstruct (rxt-char-set-intersection
            (:include rxt-syntax-tree))
  elts)

;; Intersection constructor
(defun rxt-char-set-intersection (charsets)
  (let ((elts '())
	(cmpl (make-rxt-char-set-union)))
    (dolist (cset (rxt-int-flatten charsets))
      (cond
       ((rxt-char-set-negation-p cset)
	;; Fold negated charsets together: ~A & ~B = ~(A|B)
	(setq cmpl (rxt-char-set-adjoin! cmpl (rxt-char-set-negation-elt cset))))
       
       ((rxt-char-set-union-p cset)
	(push cset elts))

       (t
	(error "Can't take intersection of non-character-set %s" cset))))
    (if (null elts)
	(rxt-negate cmpl)
      (unless (rxt-empty-char-set-p cmpl)
	(push (rxt-negate cmpl) elts))
      (if (null (cdr elts))
	  (car elts)			; singleton case
	(make-rxt-char-set-intersection :elts elts)))))
	
;; Constructor helper: flatten nested intersections
(defun rxt-int-flatten (csets)
  (if (consp csets)
      (let ((cset (car csets))
	    (tail (rxt-int-flatten (cdr csets))))
	(if (rxt-char-set-intersection-p cset)
	    (append (rxt-int-flatten (rxt-char-set-intersection-elts cset)) tail)
	  (cons cset tail)))
    '()))




;;;; ADT unparsers to rx and sre notation

;;; ADT -> rx notation
(defvar rxt-rx-verbose-equivalents
  '((bol . line-start)
    (eol . line-end)
    (nonl . not-newline)
    (bos . string-start)
    (eos . string-end)
    (bow . word-start)
    (eow . word-end)
    (seq . sequence))
  "Alist of verbose equivalents for short `rx' primitives.") 

(defun rxt-rx-symbol (sym)
  (if (or rxt-verbose-rx-translation
          (and rxt-explain
               rxt-explain-verbosely))
      (or (cdr (assoc sym rxt-rx-verbose-equivalents))
          sym)
    sym))

(defun rxt-adt->rx (re)
  (let ((rx
         (cond
          ((rxt-primitive-p re)
           (rxt-rx-symbol (rxt-primitive-rx re)))

          ((rxt-string-p re) (rxt-string-chars re))

          ((rxt-seq-p re)
           (cons (rxt-rx-symbol 'seq)
                 (mapcar #'rxt-adt->rx (rxt-seq-elts re))))

          ((rxt-choice-p re)
           (cons (rxt-rx-symbol 'or)
                 (mapcar #'rxt-adt->rx (rxt-choice-elts re))))

          ((rxt-submatch-p re)
           (if (rxt-seq-p (rxt-submatch-body re))
               (cons (rxt-rx-symbol 'submatch)
                     (mapcar #'rxt-adt->rx (rxt-seq-elts (rxt-submatch-body re))))
             (list (rxt-rx-symbol 'submatch)
                   (rxt-adt->rx (rxt-submatch-body re)))))

          ((rxt-backref-p re)
           (let ((n (rxt-backref-n re)))
             (if (<= n 9)
                 (list 'backref (rxt-backref-n re))
               (error "Too many backreferences (%s)" n))))

          ((rxt-syntax-class-p re)
           (list 'syntax (rxt-syntax-class-symbol re)))

          ((rxt-char-category-p re)
           (list 'category (rxt-char-category-symbol re)))

          ((rxt-repeat-p re)
           (let ((from (rxt-repeat-from re))
                 (to (rxt-repeat-to re))
                 (greedy (rxt-repeat-greedy re))
                 (body (rxt-adt->rx (rxt-repeat-body re))))
             (if rxt-verbose-rx-translation
                 (let ((rx
                        (cond
                         ((and (zerop from) (null to))
                          `(zero-or-more ,body))
                         ((and (equal from 1) (null to))
                          `(one-or-more ,body))
                         ((and (zerop from) (equal to 1))
                          `(zero-or-one ,body))
                         ((null to)
                          `(>= ,from ,body))
                         ((equal from to)
                          `(repeat ,from ,body))
                         (t
                          `(** ,from ,to ,body)))))
                   (if greedy
                       (if rxt-explain
                           rx           ; Readable but not strictly accurate. Fixme?
                         `(maximal-match rx))
                     `(minimal-match rx)))
               (cond
                ((and (zerop from) (null to))
                 (list (if greedy '* '*?) body))
                ((and (equal from 1) (null to))
                 (list (if greedy '+ '+?) body))
                ((and (zerop from) (equal to 1))
                 (list (if greedy '\? '\??) body))
                ((null to) (list '>= from body))
                ((equal from to)
                 (list '= from body))
                (t
                 (list '** from to body))))))

          ((rxt-char-set-union-p re)
           (if (and (null (rxt-char-set-union-chars re))
                    (null (rxt-char-set-union-ranges re))
                    (= 1 (length (rxt-char-set-union-classes re))))
               (car (rxt-char-set-union-classes re))
             (append
              '(any)
              (and (rxt-char-set-union-chars re)
                   (mapcar 'char-to-string (rxt-char-set-union-chars re)))

              (mapcar
               (lambda (range)
                 (format "%c-%c" (car range) (cdr range)))
               (rxt-char-set-union-ranges re))

              (rxt-char-set-union-classes re))))

          ((rxt-char-set-negation-p re)
           (list 'not (rxt-adt->rx (rxt-char-set-negation-elt re))))

          (t
           (error "No RX translation for %s" re)))))
    
    ;; Store source information on each fragment of the generated RX
    ;; sexp for rxt-explain mode
    (when rxt-explain
      ;; Use gensyms to store unique source information for multiple
      ;; occurrences of primitives like `bol'
      (when (symbolp rx)
        (setq rx (make-symbol (symbol-name rx))))
      (puthash rx re rxt-explain-hash-map)) 
    rx))

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
          (greedy (rxt-repeat-greedy re))
	  (body (rxt-adt->sre (rxt-repeat-body re))))
      (when (not greedy)
        (error "No SRE translation of non-greedy repetition %s" re))
      (cond
       ((and (zerop from) (null to)) (list '* body))
       ((and (equal from 1) (null to)) (list '+ body))
       ((and (zerop from) (equal to 1)) (list '\? body))
       ((null to) (list '>= from body))
       ((equal from to)
	(list '= from body))
       (t
	(list '** from to body)))))

   ((rxt-char-set-union-p re)
    (let* ((chars (mapconcat 'char-to-string (rxt-char-set-union-chars re) ""))

	   (ranges
	    (mapcar
	     (lambda (range)
	       (format "%c%c" (car range) (cdr range)))
	     (rxt-char-set-union-ranges re)))

	   (classes (rxt-char-set-union-classes re))

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

   ((rxt-char-set-intersection-p re)
    (cons '& (mapcar #'rxt-adt->sre (rxt-char-set-intersection-elts re))))

   (t
    (error "No SRE translation for %s" re))))



;;;; ADT unparser to PCRE notation
;;; Based on scsh/posixstr.scm in scsh

;; To ensure that the operator precedence in the generated regexp does
;; what we want, we need to keep track of what kind of production is
;; returned from each step. Therefore these functions return a string
;; and a numeric "level" which lets the function using the generated
;; regexp know whether it has to be parenthesized:
;;
;; 0: an already parenthesized expression
;;
;; 1: a "piece" binds to any succeeding quantifiers
;;
;; 2: a "branch", or concatenation of pieces, needs parenthesizing to
;; bind to quantifiers
;;
;; 3: a "top", or alternation of branches, needs parenthesizing to
;; bind to quantifiers or to concatenation
;;
;; This idea is stolen straight out of the scsh implementation.

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
   ((rxt-backref-p re)
    (values (format "\\%d" (rxt-backref-n re)) 1))
 
   ((rxt-repeat-p re) (rxt-repeat->pcre re))

   ((or (rxt-char-set-union-p re)
	(rxt-char-set-negation-p re))
    (rxt-char-set->pcre re))

   ;; ((rxt-char-set-intersection re) (rxt-char-set-intersection->pcre re))

   (t
    (error "No PCRE translation for %s" re))))

(defvar rxt-pcre-metachars (rx (any "\\^.$|()[]*+?{}")))
(defvar rxt-pcre-charset-metachars (rx (any "]" "[" "\\" "^" "-")))

(defun rxt-string->pcre (re)
  (values
   (replace-regexp-in-string
    rxt-pcre-metachars
    "\\\\\\&" (rxt-string-chars re))
   ;; One-char strings are pieces (bind to quantifiers), longer are
   ;; branches (need parenthesizing to be quantified)
   (if (> (length re) 1) 1 2)))

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
	(body (rxt-repeat-body re))
        (greedy (rxt-repeat-greedy re)))
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
		    (cond ((and (= from 0) (= to 1))
                           (concat s (if greedy "?" "??")))
			  ((= from to)
			   (concat s "{" (number-to-string to) "}"))
			  (t
			   (concat s "{" (number-to-string from)
                                   "," (number-to-string to) "}")))
		  (cond ((= from 0)
                         (concat s (if greedy "*" "*?")))
			((= from 1)
                         (concat s (if greedy "+" "+?")))
			(t (concat s "{" (number-to-string from) ",}"))))
		1))))))
	
(defun rxt-char-set->pcre (re)
  (cond ((rxt-char-set-union-p re)
	 (values
	  (concat "[" (rxt-char-set->pcre/chars re) "]") 1))

	((rxt-char-set-negation-p re)
	 (let ((elt (rxt-char-set-negation-elt re)))
	   (if (rxt-char-set-union-p elt)
	       (values
		(concat "[^" (rxt-char-set->pcre/chars elt) "]") 1)
	     (error "No PCRE translation of %s" elt))))

	(t
	 (error "Non-char-set in rxt-char-set->pcre: %s" re))))
	 
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

    (let ((chars (rxt-char-set-union-chars re))
	  (ranges (rxt-char-set-union-ranges re))
	  (classes (rxt-char-set-union-classes re)))

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


;;;; Generate all productions of a finite regexp

(defun rxt-adt->strings (re)
  (cond
   ((rxt-primitive-p re) (list ""))

   ((rxt-string-p re) (list (rxt-string-chars re)))
   ((rxt-seq-p re) (rxt-seq-elts->strings (rxt-seq-elts re)))
   ((rxt-choice-p re) (rxt-choice-elts->strings (rxt-choice-elts re)))

   ((rxt-submatch-p re) (rxt-adt->strings (rxt-submatch-body re)))

   ((rxt-repeat-p re) (rxt-repeat->strings re))

   ((rxt-char-set-union-p re) (rxt-char-set->strings re))

   (t
    (error "Can't generate matches for %s" re))))

(defun rxt-concat-product (heads tails)
  (mapcan
   (lambda (hs)
     (mapcar
      (lambda (ts) (concat hs ts))
      tails))
   heads))

(defun rxt-seq-elts->strings (elts)
  (if (null elts)
      '("")
    (let ((heads (rxt-adt->strings (car elts)))
	  (tails (rxt-seq-elts->strings (cdr elts))))
      (rxt-concat-product heads tails))))

(defun rxt-choice-elts->strings (elts)
  (if (null elts)
      '()
    (append (rxt-adt->strings (car elts))
	    (rxt-choice-elts->strings (cdr elts)))))

(defun rxt-repeat->strings (re)
  (let ((from (rxt-repeat-from re))
	(to (rxt-repeat-to re)))
    (if (not to)
	(error "Can't generate matches for unbounded repeat %s"
               re)
      (let ((strings (rxt-adt->strings (rxt-repeat-body re))))
	(rxt-repeat-n-m->strings from to strings)))))

(defun rxt-repeat-n-m->strings (from to strings)
  (cond
   ((zerop to) '(""))
   ((= to from) (rxt-repeat-n->strings from strings))
   (t 					; to > from
    (let* ((strs-n (rxt-repeat-n->strings from strings))
	   (accum (copy-list strs-n)))
      (dotimes (i (- to from))
	(setq strs-n (rxt-concat-product strs-n strings))
	(setq accum (nconc accum strs-n)))
      accum))))
	      
(defun rxt-repeat-n->strings (n strings)
  ;; n > 1
  (cond ((zerop n) '(""))
	((= n 1) strings)
	(t
	 (rxt-concat-product
	  (rxt-repeat-n->strings (- n 1) strings)
	  strings))))

(defun rxt-char-set->strings (re)
  (if (rxt-char-set-union-classes re)
      (error "Can't generate matches for character classes")
    (let ((chars (mapcar #'char-to-string (rxt-char-set-union-chars re))))
      (dolist (range (rxt-char-set-union-ranges re))
	(let ((end (cdr range)))
	  (do ((i (car range) (+ i 1)))
	      ((> i end))
	    (push (char-to-string i) chars))))
      chars)))


;;;; String regexp -> ADT parser

(defvar rxt-parse-pcre nil
  "t if the rxt string parser is parsing PCRE syntax, nil for Elisp syntax.

This should only be let-bound by `rxt-parse-re', never set otherwise.")

(defvar rxt-pcre-extended-mode nil
  "t if the rxt string parser is emulating PCRE's \"extended\" mode.

In extended mode (indicated by /x in Perl/PCRE), whitespace
outside of character classes and \\Q...\\E quoting is ignored,
and a `#' character introduces a comment that extends to the end
of line.")

(defvar rxt-pcre-s-mode nil
  "t if the rxt string parser is emulating PCRE's single-line \"/s\" mode.

When /s is used, PCRE's \".\" matches newline characters, which
otherwise it would not match.")

(defvar rxt-branch-stop-regexp nil)
(defvar rxt-choice-regexp nil)

(defvar rxt-source-text-string nil)

(defun rxt-parse-re (re &optional pcre flags)
  (let* ((rxt-parse-pcre pcre)
         (rxt-pcre-extended-mode
          (and pcre (stringp flags) (rxt-extended-flag flags)))
         (rxt-pcre-s-mode
          (and pcre (stringp flags) (rxt-s-flag flags)))

         (rxt-choice-regexp
          (if pcre (rx "|") (rx "\\|")))
         (rxt-branch-stop-regexp
          (if pcre
              (rx (or "|" ")"))
            (rx (or "\\|" "\\)"))))

         (rxt-subgroup-count 0)
         (case-fold-search nil))
    (with-temp-buffer
      (insert re)
      (goto-char (point-min))
      (let ((rxt-source-text-string re))
        (rxt-parse-exp)))))
      
;; Read PCRE + flags
(defun rxt-read-delimited-pcre ()
  "Read a Perl-style delimited regexp and flags from the current buffer.

Point should be before the regexp literal before calling
this. Currently only regexps delimited by / ... / are supported.
A preceding \"m\", \"qr\" or \"s\" will be ignored, as will the
replacement string in an s/.../.../ construction.

Returns two strings: the regexp and the flags."
  (save-excursion
    (skip-syntax-forward "-")

    ;; Skip m, qr, s
    (let ((is-subst (rxt-token-case
                     ("s" t)
                     ((rx (or "m" "qr")) nil))))

      (when (not (looking-at "/"))
        (error "Only Perl regexps delimited by slashes are supported"))
      (let ((beg (match-end 0))
            (delim (rx (not (any "\\"))
                       (group "/"))))
        (search-forward-regexp delim)
        (let ((end (match-beginning 1)))
          (when is-subst (search-forward-regexp delim))
        (let ((pcre (buffer-substring-no-properties beg end)))
          (rxt-token-case
           ("[gimosx]*"
            (values pcre (match-string-no-properties 0))))))))))

(defmacro rxt-syntax-tree-value (&rest body)
  (declare (debug (&rest form)))
  (let ((begin (make-symbol "begin"))
        (value (make-symbol "value")))
    `(let ((,begin (point))
           (,value
            (progn ,@body)))
       (when (rxt-primitive-p ,value)
         (setq ,value (copy-rxt-primitive ,value)))
       (setf (rxt-syntax-tree-begin ,value) (1- ,begin)
             (rxt-syntax-tree-end ,value) (1- (point))
             (rxt-syntax-tree-source ,value) rxt-source-text-string)
       ,value)))

;; Parse a complete regex: a number of branches separated by | or
;; \|, as determined by `rxt-branch-stop-regexp'.
(defun rxt-parse-exp ()
  ;; These variables are let-bound here because in PCRE mode they may
  ;; be set internally by (?x) or (?s) constructions, whose scope
  ;; lasts until the end of a sub-expression
  (rxt-syntax-tree-value
   (let ((rxt-pcre-extended-mode rxt-pcre-extended-mode)
         (rxt-pcre-s-mode rxt-pcre-s-mode))
     (if (not (eobp))
         (let ((branches '()))
           (catch 'done
             (while t
               (let ((branch (rxt-parse-branch)))
                 (push branch branches)
                 (if (looking-at rxt-choice-regexp)
                     (goto-char (match-end 0))
                   (throw 'done (rxt-choice (reverse branches))))))))
       (rxt-seq nil)))))

;; Skip over whitespace and comments in PCRE extended mode
(defun rxt-extended-skip ()
  (when rxt-pcre-extended-mode
    (skip-syntax-forward "-")
    (while (looking-at "#")
      (beginning-of-line 2)
      (skip-syntax-forward "-"))))

;; Parse a regexp "branch": a sequence of pieces
(defun rxt-parse-branch ()
  (rxt-extended-skip)
  (rxt-syntax-tree-value
   (let ((pieces (list (rxt-parse-piece t))))
     (while (not (or (eobp)
                     (looking-at rxt-branch-stop-regexp)))
       (let ((piece (rxt-parse-piece nil)))
         (push piece pieces)))
     (rxt-seq (reverse pieces)))))

;; Parse a regexp "piece": an atom (`rxt-parse-atom') plus any
;; following quantifiers
(defun rxt-parse-piece (&optional branch-begin)
  (rxt-extended-skip)
  (rxt-syntax-tree-value
   (let ((atom (rxt-parse-atom branch-begin)))
     (rxt-parse-quantifiers atom))))

;; Parse any and all quantifiers after ATOM and return the quantified
;; regexp, or ATOM unchanged if no quantifiers
(defun rxt-parse-quantifiers (atom)
  (catch 'done
    (while (not (eobp))
      (let ((atom1 (rxt-parse-quantifier atom)))
        (if (eq atom1 atom)
            (throw 'done t)
          (setq atom atom1)))))
  atom)

;; Possibly parse a single quantifier after ATOM and return the
;; quantified atom, or ATOM if no quantifier
(defun rxt-parse-quantifier (atom)
  (rxt-extended-skip)
  (rxt-token-case
   ((rx "*?") (rxt-repeat 0 nil atom nil))
   ((rx "*") (rxt-repeat 0 nil atom t))

   ((rx "+?") (rxt-repeat 1 nil atom nil))
   ((rx "+") (rxt-repeat 1 nil atom t))

   ((rx "??") (rxt-repeat 0 1 atom nil))
   ((rx "?") (rxt-repeat 0 1 atom t))

   ((if rxt-parse-pcre "{" "\\\\{")
    (multiple-value-bind (from to)
	(rxt-parse-braces)
      (rxt-repeat from to atom)))

   (t atom)))

;; Parse a regexp atom, i.e. an element that binds to any following
;; quantifiers. This includes characters, character classes,
;; parenthesized groups, assertions, etc.
(defun rxt-parse-atom (&optional branch-begin)
  (if (eobp)
      (error "Unexpected end of regular expression")
    (if rxt-parse-pcre
        (rxt-parse-atom/pcre)
      (rxt-parse-atom/el branch-begin))))

(defun rxt-parse-atom/common ()
  (rxt-token-case 
   ("\\[" (rxt-parse-char-class))
   ("\\\\b" rxt-word-boundary)
   ("\\\\B" rxt-not-word-boundary)))

(defun rxt-parse-atom/el (branch-begin)
  (rxt-syntax-tree-value
   (or (rxt-parse-atom/common)
       (rxt-token-case
        ("\\." rxt-nonl)

        ;; "^" and "$" are metacharacters only at beginning or end of a
        ;; branch in Elisp; elsewhere they are literals
        ("\\^" (if branch-begin
                   rxt-bol
                 (rxt-string "^")))
        ("\\$" (if (or (eobp)
                       (looking-at (rx (or "\\)" "\\|"))))
                   rxt-eol
                 (rxt-string "$")))

        ("\\\\(" (rxt-parse-subgroup/el)) ; Subgroup
   
        ("\\\\w" rxt-wordchar)
        ("\\\\W" rxt-not-wordchar)

        ("\\\\`" rxt-bos)
        ("\\\\'" rxt-eos)
        ("\\\\<" rxt-bow)
        ("\\\\>" rxt-eow)
        ("\\\\_<" rxt-symbol-start)
        ("\\\\_>" rxt-symbol-end)

        ;; Syntax categories
        ((rx "\\"
             (submatch (any "Ss"))
             (submatch (any "-.w_()'\"$\\/<>|!")))
         (let ((negated (string= (match-string 1) "S"))
               (re
                (rxt-syntax-class
                 (car (rassoc (string-to-char (match-string 2))
                              rx-syntax)))))
           (if negated (rxt-negate re) re)))

        ;; Character categories
        ((rx "\\"
             (submatch (any "Cc"))
             (submatch nonl))
         (let ((negated (string= (match-string 1) "C"))
               (category
                (car (rassoc (string-to-char (match-string 2))
                             rx-categories))))
           (unless category
             (error "Unrecognized character category %s" (match-string 2)))
           (let ((re (rxt-char-category category)))
             (if negated (rxt-negate re) re))))

        ("\\\\\\([1-9]\\)"              ; Backreference
         (rxt-backref (string-to-number (match-string 1))))

        ;; Any other escaped character
        ("\\\\\\(.\\)" (rxt-string (match-string 1)))

        ;; Everything else
        (".\\|\n" (rxt-string (match-string 0)))))))

(defvar rxt-subgroup-count nil)

(defvar rxt-pcre-word-chars
  (make-rxt-char-set-union :chars '(?_)
                     :classes '(alnum)))

(defvar rxt-pcre-non-word-chars
  (rxt-negate rxt-pcre-word-chars))

(defun rxt-parse-atom/pcre ()
  (rxt-extended-skip)
  (rxt-syntax-tree-value
   (or (rxt-parse-atom/common)

       (let ((char (rxt-parse-escapes/pcre)))
         (and char
              (rxt-string (char-to-string char))))

       (rxt-token-case
        ("\\."
         (if rxt-pcre-s-mode
             rxt-anything
           rxt-nonl))

        ("\\^" rxt-bol)
        ("\\$" rxt-eol)

        ("(" (rxt-parse-subgroup/pcre)) ; Subgroup
       
        ("\\\\A" rxt-bos)
        ("\\\\Z" rxt-eos)

        ("\\\\w" rxt-pcre-word-chars)
        ("\\\\W" rxt-pcre-non-word-chars)

        ("\\\\Q"                        ; begin regexp quoting
         ;; It would seem simple to take all the characters between \Q
         ;; and \E and make an rxt-string, but \Q...\E isn't an atom:
         ;; any quantifiers afterward should bind only to the last
         ;; character, not the whole string.
         (let ((begin (point)))
           (search-forward "\\E" nil t)
           (let* ((end (match-beginning 0))
                  (str (buffer-substring-no-properties begin (1- end)))
                  (char (char-to-string (char-before end))))
             (rxt-seq (list (rxt-string str)
                            (rxt-parse-quantifiers (rxt-string char)))))))
       
        ;; Various character classes
        ("\\\\d" (rxt-simple-char-set 'digit))
        ("\\\\D" (rxt-negate 'digit))
        ("\\\\h" (rxt-simple-char-set rxt-pcre-horizontal-whitespace-chars))
        ("\\\\H" (rxt-negate rxt-pcre-horizontal-whitespace-chars))
        ("\\\\s" (rxt-simple-char-set 'space))
        ("\\\\S" (rxt-negate 'space))
        ("\\\\v" (rxt-simple-char-set rxt-pcre-vertical-whitespace-chars))
        ("\\\\V" (rxt-negate rxt-pcre-vertical-whitespace-chars))

        ("\\\\\\([0-9]+\\)"		; backreference or octal char?
         (let* ((digits (match-string 1))
                (dec (string-to-number digits)))
           ;; from "man pcrepattern": If the number is less than 10, or if
           ;; there have been at least that many previous capturing left
           ;; parentheses in the expression, the entire sequence is taken
           ;; as a back reference.
           (if (and (> dec 0)
                    (or (< dec 10)
                        (>= rxt-subgroup-count dec)))
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
        (".\\|\n" (rxt-string (match-string 0)))))))

(defun rxt-parse-escapes/pcre ()
  "Consume a one-char PCRE escape at point and return its codepoint equivalent.

Handles only those character escapes which have the same meaning
in character classes as outside them."
  (rxt-token-case
   ("\\\\a" #x07)  ; bell
   ("\\\\c\\(.\\)"                  ; control character
    ;; from `man pcrepattern':
    ;; The precise effect of \cx is as follows: if x is a lower case
    ;; letter, it is converted to upper case.  Then bit 6 of the
    ;; character (hex 40) is inverted.
    (logxor (string-to-char (upcase (match-string 1))) #x40))
   ("\\\\e" #x1b)  ; escape
   ("\\\\f" #x0c)  ; formfeed
   ("\\\\n" #x0a)  ; linefeed
   ("\\\\r" #x0d)  ; carriage return
   ("\\\\t" #x09)  ; tab
   
   ("\\\\x\\([A-Za-z0-9]\\{1,2\\}\\)"
    (string-to-number (match-string 1) 16))
   ("\\\\x{\\([A-Za-z0-9]*\\)}"
    (string-to-number (match-string 1) 16))))

(defun rxt-extended-flag (flags)
  (if (string-match-p "x" flags) t nil))

(defun rxt-s-flag (flags)
  (if (string-match-p "s" flags) t nil))

(defun rxt-parse-subgroup/pcre ()
  (catch 'return 
    (let ((shy nil)
          (x rxt-pcre-extended-mode)
          (s rxt-pcre-s-mode))
      (rxt-extended-skip)
      ;; Check for special constructs (? ... ) and (* ...)
      (rxt-token-case
       ((rx "?")                        ; (? ... )
        (rxt-token-case
         (":" (setq shy t))             ; Shy group (?: ...)
         ("#"                           ; Comment (?# ...)
          (search-forward ")")
          (throw 'return rxt-empty-string))
         ((rx (or                       ; Set/unset s & x modifiers
               (seq (group (* (any "xs"))) "-" (group (+ (any "xs"))))
               (seq (group (+ (any "xs"))))))
          (let ((begin (match-beginning 0))
                (on (or (match-string 1) (match-string 3)))
                (off (or (match-string 2) "")))
            (if (rxt-extended-flag on) (setq x t))
            (if (rxt-s-flag on) (setq s t))
            (if (rxt-extended-flag off) (setq x nil))
            (if (rxt-s-flag off) (setq s nil))
            (rxt-token-case
             (":" (setq shy t))   ; Parse a shy group with these flags
             (")"
              ;; Set modifiers here to take effect for the remainder
              ;; of this expression; they are let-bound in
              ;; rxt-parse-exp
              (setq rxt-pcre-extended-mode x
                    rxt-pcre-s-mode s)
              (throw 'return rxt-empty-string))
             (t
              (error "Unrecognized PCRE extended construction (?%s...)"
                     (buffer-substring-no-properties begin (point)))))))
         (t (error "Unrecognized PCRE extended construction ?%c"
                   (char-after)))))

       ((rx "*")                ; None of these are recognized: (* ..)
        (let ((begin (point)))
          (search-forward ")")
          (error "Unrecognized PCRE extended construction (*%s"
                 (buffer-substring begin (point))))))

      ;; Parse the remainder of the subgroup
      (unless shy (incf rxt-subgroup-count))
      (let* ((rxt-pcre-extended-mode x)
             (rxt-pcre-s-mode s)
             (rx (rxt-parse-exp)))
        (rxt-extended-skip)
        (rxt-token-case
         (")" (if shy rx (rxt-submatch rx)))
         (t (error "Subexpression missing close paren")))))))

(defun rxt-parse-subgroup/el ()
  (let ((shy (rxt-token-case ("\\?:" t))))
    (unless shy (incf rxt-subgroup-count))
    (let ((rx (rxt-parse-exp)))
      (rxt-token-case
       ((rx "\\)") (if shy rx (rxt-submatch rx)))
       (t (error "Subexpression missing close paren"))))))

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
    (let ((begin (point)))
      (search-forward "}" nil 'go-to-end)
      (error "Bad brace expression {%s"
             (buffer-substring-no-properties begin (point)))))))

;; Parse a character set range [...]		 
(defvar rxt-posix-classes
  (rx "[:"
      (submatch
       (or "alnum" "alpha" "ascii" "blank" "cntrl" "digit" "graph" "lower"
           "print" "punct" "space" "upper" "word" "xdigit"))
      ":]"))

(defun rxt-parse-char-class ()
  (when (eobp)
    (error "Missing close right bracket in regexp"))

  (rxt-syntax-tree-value
   (let* ((negated (rxt-token-case
                    ("\\^" t)
                    (t nil)))
          (begin (point))
          (result (if negated
                      (rxt-negate (make-rxt-char-set-union))
                    (make-rxt-char-set-union)))
          (builder (if negated
                       #'rxt-char-set-intersection
                     #'rxt-char-set-union)))
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
                       (rxt-negate (rxt-simple-char-set piece))
                     (rxt-simple-char-set piece))))
             (setq result
                   (funcall builder (list result cset)))))))
     ;; Skip over closing "]"
     (forward-char)
     result)))

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

       ("\\\\h" (rxt-simple-char-set rxt-pcre-horizontal-whitespace-chars))
       ("\\\\H" (rxt-negate rxt-pcre-horizontal-whitespace-chars))

       ("\\\\s" (rxt-simple-char-set rxt-pcre-whitespace-chars))
       ("\\\\S" (rxt-negate rxt-pcre-whitespace-chars))

       ("\\\\v" (rxt-simple-char-set rxt-pcre-vertical-whitespace-chars))
       ("\\\\V" (rxt-negate rxt-pcre-vertical-whitespace-chars))

       ("\\\\w" rxt-pcre-word-chars)
       ("\\\\W" rxt-pcre-non-word-chars)

       ;; \b inside character classes = backspace
       ("\\\\b" ?\C-h)

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



;;;; RE-Builder hacks

;;; These are implemented as advice so that they can be unloaded and
;;; restore the original `re-builder' versions. However, each one is
;;; basically a complete replacement for the function of the same name
;;; (and most of the code is taken from re-builder.el).

(defadvice reb-update-modestring
  (around rxt () activate compile)
  "This function is hacked for emulated PCRE syntax and regexp conversion."
  (setq reb-mode-string
	(concat
	 (format " (%s)" reb-re-syntax)
	 (if reb-subexp-mode
             (format " (subexp %s)" (or reb-subexp-displayed "-"))
	   "")
	 (if (not (reb-target-binding case-fold-search))
	     " Case"
	   "")))
  (force-mode-line-update))

(defadvice reb-change-syntax
  (around rxt (&optional syntax) activate compile)
  "This function is hacked for emulated PCRE syntax and regexp conversion."  
  (interactive
   (list (intern
	  (completing-read (format "Select syntax (%s): " reb-re-syntax)
			   '("read" "string" "pcre" "sregex" "rx")
			   nil t "" nil (symbol-name reb-re-syntax)))))
  (setq ad-return-value
        (if (memq syntax '(read string pcre lisp-re sregex rx))
            (let ((buffer (get-buffer reb-buffer)))
              (setq reb-re-syntax syntax)
              (when buffer
                (with-current-buffer reb-target-buffer
                  (case syntax
                    ((rx)
                     (setq reb-regexp-src
                           (format "'%S"
                                   (rxt-elisp-to-rx reb-regexp))))
                    ((pcre)
                     (setq reb-regexp-src
                           (list (rxt-elisp-to-pcre reb-regexp) "")))))
                (with-current-buffer buffer
                  ;; Hack: prevent reb-auto-update from clobbering the
                  ;; reb-regexp-src we just set
                  (let ((inhibit-modification-hooks t))
                    (reb-initialize-buffer)))))
          (error "Invalid syntax: %s" syntax))))

(defadvice reb-read-regexp
  (around rxt () activate compile)
  "This function is hacked for emulated PCRE syntax and regexp conversion."
  (setq ad-return-value
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
                 (rxt-read-delimited-pcre))

                ((or (reb-lisp-syntax-p) (eq reb-re-syntax 'pcre))
                 (buffer-string))))))

(defadvice reb-insert-regexp
  (around rxt () activate compile)
  "This function is hacked for emulated PCRE syntax and regexp conversion."
  (setq ad-return-value
        (let ((re (or (reb-target-binding reb-regexp)
                      (reb-empty-regexp))))
          (cond ((eq reb-re-syntax 'read)
                 (print re (current-buffer)))
                ;; For PCRE syntax the value of reb-regexp-src is a
                ;; list (REGEXP FLAGS)
                ((eq reb-re-syntax 'pcre)
                 (let ((src (reb-target-binding reb-regexp-src)))
                   (if (not src)
                       (insert "\n//")
                     (insert "\n/" (car src) "/" (cadr src)))))

                ((eq reb-re-syntax 'string)
                 (insert "\n\"" re "\""))

                ;; For the Lisp syntax we need the "source" of the regexp 
                ((reb-lisp-syntax-p)
                 (insert (or (reb-target-binding reb-regexp-src)
                             (reb-empty-regexp))))))))

(defadvice reb-cook-regexp
  (around rxt (re) activate compile)
  "This function is hacked for emulated PCRE syntax and regexp conversion."
  (setq ad-return-value
        (cond ((eq reb-re-syntax 'lisp-re)
               (when (fboundp 'lre-compile-string)
                 (lre-compile-string (eval (car (read-from-string re))))))

              ((eq reb-re-syntax 'sregex)
               (apply 'sregex (eval (car (read-from-string re)))))

              ((eq reb-re-syntax 'rx)
               (rx-to-string (eval (car (read-from-string re))) t))
              
              ((eq reb-re-syntax 'pcre)
               (rxt-pcre-to-elisp (car re) (cadr re)))

              (t re))))

(defadvice reb-update-regexp
  (around rxt () activate compile)
  "This function is hacked for emulated PCRE syntax and regexp conversion."
  (setq ad-return-value
        (let* ((re-src (reb-read-regexp))
               (re (reb-cook-regexp re-src)))
          (with-current-buffer reb-target-buffer
            (let ((oldre reb-regexp))
              (prog1
                  (not (string= oldre re))
                (setq reb-regexp re)
                ;; Only update the source re for the lisp formats
                (when (or (reb-lisp-syntax-p) (eq reb-re-syntax 'pcre))
                  (setq reb-regexp-src re-src))))))))

(provide 'rxt)
(provide 'pcre2el)


;;; pcre2el.el ends here

