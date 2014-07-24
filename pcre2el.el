;;; pcre2el.el --- parse, convert, and font-lock PCRE, Emacs and rx regexps

;; Copyright (C) 2012-2014 Jon Oddie <jonxfield@gmail.com>

;; Author:			joddie <jonxfield at gmail.com>
;; Hacked additionally by:	opensource at hardakers dot net
;; Created:			14 Feb 2012
;; Updated:			24 July 2014
;; Version:                     1.8
;; Url:                         https://github.com/joddie/pcre2el
;; Package-Requires:            ((cl-lib "0.3"))

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

;; 1 Overview
;; ==========

;;   `pcre2el' or `rxt' (RegeXp Translator or RegeXp Tools) is a utility
;;   for working with regular expressions in Emacs, based on a
;;   recursive-descent parser for regexp syntax. In addition to converting
;;   (a subset of) PCRE syntax into its Emacs equivalent, it can do the
;;   following:

;;   - convert Emacs syntax to PCRE
;;   - convert either syntax to `rx', an S-expression based regexp syntax
;;   - untangle complex regexps by showing the parse tree in `rx' form and
;;     highlighting the corresponding chunks of code
;;   - show the complete list of strings (productions) matching a regexp,
;;     provided the list is finite
;;   - provide live font-locking of regexp syntax (so far only for Elisp
;;     buffers -- other modes on the TODO list)


;; 2 Usage
;; =======

;;   Enable `rxt-mode' or its global equivalent `rxt-global-mode' to get
;;   the default key-bindings. There are three sets of commands: commands
;;   that take a PCRE regexp, commands which take an Emacs regexp, and
;;   commands that try to do the right thing based on the current
;;   mode. Currently, this means Emacs syntax in `emacs-lisp-mode' and
;;   `lisp-interaction-mode', and PCRE syntax everywhere else.

;;   The default key bindings all begin with `C-c /' and have a mnemonic
;;   structure: `C-c / <source> <target>', or just `C-c / <target>' for the
;;   "do what I mean" commands. The complete list of key bindings is given
;;   here and explained in more detail below:

;;   - "Do-what-I-mean" commands:
;;     `C-c / /': `rxt-explain'
;;     `C-c / c': `rxt-convert-syntax'
;;     `C-c / x': `rxt-convert-to-rx'
;;     `C-c / '': `rxt-convert-to-strings'

;;   - Commands that work on a PCRE regexp:
;;     `C-c / p e': `rxt-pcre-to-elisp'
;;     `C-c / %': `pcre-query-replace-regexp'
;;     `C-c / p x': `rxt-pcre-to-rx'
;;     `C-c / p s': `rxt-pcre-to-sre'
;;     `C-c / p '': `rxt-pcre-to-strings'
;;     `C-c / p /': `rxt-explain-pcre'

;;   - Commands that work on an Emacs regexp:
;;     `C-c / e /': `rxt-explain-elisp'
;;     `C-c / e p': `rxt-elisp-to-pcre'
;;     `C-c / e x': `rxt-elisp-to-rx'
;;     `C-c / e s': `rxt-elisp-to-sre'
;;     `C-c / e '': `rxt-elisp-to-strings'
;;     `C-c / e t': `rxt-toggle-elisp-rx'
;;     `C-c / t': `rxt-toggle-elisp-rx'
;;     `C-c / h': `rxt-fontify-regexp-at-point'


;; 2.1 Interactive input and output
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   When used interactively, the conversion commands can read a regexp
;;   either from the current buffer or from the minibuffer. The output is
;;   displayed in the minibuffer and copied to the kill-ring.

;;   - When called with a prefix argument (`C-u'), they read a regular
;;     expression from the minibuffer literally, without further processing
;;     -- meaning there's no need to double the backslashes if it's an
;;     Emacs regexp.  This is the same way commands like
;;     `query-replace-regexp' read input.

;;   - When the region is active, they use they the region contents, again
;;     literally (without any translation of string syntax).

;;   - With neither a prefix arg nor an active region, the behavior depends
;;     on whether the command expects an Emacs regexp or a PCRE one.

;;     Commands that take an Emacs regexp behave like `C-x C-e': they
;;     evaluate the sexp before point (which could be simply a string
;;     literal) and use its value. This is designed for use in Elisp
;;     buffers. As a special case, if point is *inside* a string, it's
;;     first moved to the string end, so in practice they should work as
;;     long as point is somewhere within the regexp literal.

;;     Commands that take a PCRE regexp try to read a Perl-style delimited
;;     regex literal *after* point in the current buffer, including its
;;     flags. For example, putting point before the `m' in the following
;;     example and doing `C-c / p e' (`rxt-pcre-to-elisp') displays
;;     `\(?:bar\|foo\)', correctly stripping out the whitespace and
;;     comment:

;;     ,----
;;     | $x =~ m/  foo   |  (?# comment) bar /x
;;     `----

;;     The PCRE reader currently only works with `/ ... /' delimiters. It
;;     will ignore any preceding `m', `s', or `qr' operator, as well as the
;;     replacement part of an `s' construction.

;;     Readers for other PCRE-using languages are on the TODO list.

;;   The translation functions display their result in the minibuffer and
;;   copy it to the kill ring. When translating something into Elisp
;;   syntax, you might need to use the result either literally (e.g. for
;;   interactive input to a command like `query-replace-regexp'), or as a
;;   string to paste into Lisp code.  To allow both uses,
;;   `rxt-pcre-to-elisp' copies both versions successively to the
;;   kill-ring. The literal regexp without string quoting is the top
;;   element of the kill-ring, while the Lisp string is the
;;   second-from-top. You can paste the literal regexp somewhere by doing
;;   `C-y', or the Lisp string by `C-y M-y'.


;; 2.2 Syntax conversion commands
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   `rxt-convert-syntax' (`C-c / c') converts between Emacs and PCRE
;;   syntax, depending on the major mode in effect when called.
;;   Alternatively, you can specify the conversion direction explicitly by
;;   using either `rxt-pcre-to-elisp' (`C-c / p e') or `rxt-elisp-to-pcre'
;;   (`C-c / e p').

;;   Similarly, `rxt-convert-to-rx' (`C-c / x') converts either kind of
;;   syntax to `rx' form, while `rxt-convert-pcre-to-rx' (`C-c / p x') and
;;   `rxt-convert-elisp-to-rx' (`C-c / e x') convert to `rx' from a
;;   specified source type.

;;   In Elisp buffers, you can use `rxt-toggle-elisp-rx' (`C-c / t' or `C-c
;;   / e t') to switch the regexp at point back and forth between string
;;   and `rx' syntax. Point should either be within an `rx' or
;;   `rx-to-string' form or a string literal for this to work.


;; 2.3 PCRE mode (experimental)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   If you want to use emulated PCRE regexp syntax in all Emacs commands,
;;   try `pcre-mode', which uses Emacs's advice system to make all commands
;;   that read regexps using the minibuffer use emulated PCRE syntax.  It
;;   should also work with Isearch.

;;   This feature is still fairly experimental.  It may fail to work or do
;;   the wrong thing with certain commands.  Please report bugs.

;;   `pcre-query-replace-regexp' was originally defined to do query-replace
;;   using emulated PCRE regexps, and is now made somewhat obsolete by
;;   `pcre-mode'.  It is bound to `C-c / %' by default, by analogy with
;;   `M-%'.  Put the following in your `.emacs' if you want to use
;;   PCRE-style query replacement everywhere:

;;   ,----
;;   | (global-set-key [(meta %)] 'pcre-query-replace-regexp)
;;   `----


;; 2.4 Syntax highlighting (font-lock)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   In Elisp buffers, you can have a regular expression in a string
;;   syntax-highlighted by putting point on it and doing
;;   `rxt-fontify-regexp-at-point' (`C-c / h'). Call the command a second
;;   time to remove the highlighting, or call with a prefix argument to
;;   remove all regexp highlighting in a buffer.

;;   As long as syntax highlighting is enabled, any edits to the string are
;;   highlighted "live" after a small delay. You can have as many strings
;;   highlighted at once as you like, but too many might slow down display.

;;   This feature doesn't work for any other language modes yet, but it
;;   would be easy to implement.


;; 2.5 Explain regexps
;; ~~~~~~~~~~~~~~~~~~~

;;   When syntax-highlighting isn't enough to untangle some gnarly regexp
;;   you find in the wild, try the 'explain' commands: `rxt-explain' (`C-c
;;   / /'), `rxt-explain-pcre' (`C-c / p') and `rxt-explain-elisp' (`C-c /
;;   e'). These display the original regexp along with its pretty-printed
;;   `rx' equivalent in a new buffer.  Moving point around either in the
;;   original regexp or the `rx' translation highlights corresponding
;;   pieces of syntax, which can aid in seeing things like the scope of
;;   quantifiers.

;;   I call them "explain" commands because the `rx' form is close to a
;;   plain syntax tree, and this plus the wordiness of the operators
;;   usually helps to clarify what is going on.  People who dislike Lisp
;;   syntax might disagree with this assessment.


;; 2.6 Generate all matching strings (productions)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   Occasionally you come across a regexp which is designed to match a
;;   finite set of strings, e.g. a set of keywords, and it would be useful
;;   to recover the original set. (In Emacs you can generate such regexps
;;   using `regexp-opt'). The commands `rxt-convert-to-strings' (`C-c /
;;   ′'), `rxt-pcre-to-strings' (`C-c / p ′') or `rxt-elisp-to-strings'
;;   (`C-c / e ′') accomplish this by generating all the matching strings
;;   ("productions") of a regexp.  (The productions are copied to the kill
;;   ring as a Lisp list).

;;   An example in Lisp code:

;;   ,----
;;   | (regexp-opt '("cat" "caterpillar" "catatonic"))
;;   |    ;; => "\\(?:cat\\(?:atonic\\|erpillar\\)?\\)"
;;   | (rxt-elisp-to-strings "\\(?:cat\\(?:atonic\\|erpillar\\)?\\)")
;;   |     ;; => '("cat" "caterpillar" "catatonic")
;;   `----

;;   For obvious reasons, these commands only work with regexps that don't
;;   include any unbounded quantifiers like `+' or `*'. They also can't
;;   enumerate all the characters that match a named character class like
;;   `[[:alnum:]]'. In either case they will give a (hopefully meaningful)
;;   error message. Due to the nature of permutations, it's still possible
;;   for a finite regexp to generate a huge number of productions, which
;;   will eat memory and slow down your Emacs. Be ready with `C-g' if
;;   necessary.


;; 2.7 RE-Builder support
;; ~~~~~~~~~~~~~~~~~~~~~~

;;   The Emacs RE-Builder is a useful visual tool which allows using
;;   several different built-in syntaxes via `reb-change-syntax' (`C-c
;;   TAB'). It supports Elisp read and literal syntax and `rx', but it can
;;   only convert from the symbolic forms to Elisp, not the other way. This
;;   package hacks the RE-Builder to also work with emulated PCRE syntax,
;;   and to convert transparently between Elisp, PCRE and rx syntaxes. PCRE
;;   mode reads a delimited Perl-like literal of the form `/ ... /', and it
;;   should correctly support using the `x' and `s' flags.


;; 2.8 Use from Lisp
;; ~~~~~~~~~~~~~~~~~

;;   Example of using the conversion functions:
;;   ,----
;;   | (rxt-pcre-to-elisp "(abc|def)\\w+\\d+")
;;   |    ;; => "\\(\\(?:abc\\|def\\)\\)[_[:alnum:]]+[[:digit:]]+"
;;   `----

;;   All the conversion functions take a single string argument, the regexp
;;   to translate:

;;   - `rxt-pcre-to-elisp'
;;   - `rxt-pcre-to-rx'
;;   - `rxt-pcre-to-sre'
;;   - `rxt-pcre-to-strings'
;;   - `rxt-elisp-to-pcre'
;;   - `rxt-elisp-to-rx'
;;   - `rxt-elisp-to-sre'
;;   - `rxt-elisp-to-strings'


;; 3 Bugs and Limitations
;; ======================

;; 3.1 Limitations on PCRE syntax
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   PCRE has a complicated syntax and semantics, only some of which can be
;;   translated into Elisp. The following subset of PCRE should be
;;   correctly parsed and converted:

;;   - parenthesis grouping `( .. )', including shy matches `(?: ... )'
;;   - backreferences (various syntaxes), but only up to 9 per expression
;;   - alternation `|'
;;   - greedy and non-greedy quantifiers `*', `*?', `+', `+?', `?' and `??'
;;           (all of which are the same in Elisp as in PCRE)
;;   - numerical quantifiers `{M,N}'
;;   - beginning/end of string `\A', `\Z'
;;   - string quoting `\Q .. \E'
;;   - word boundaries `\b', `\B' (these are the same in Elisp)
;;   - single character escapes `\a', `\c', `\e', `\f', `\n', `\r', `\t',
;;     `\x', and `\octal digits' (but see below about non-ASCII characters)
;;   - character classes `[...]' including Posix escapes
;;   - character classes `\d', `\D', `\h', `\H', `\s', `\S', `\v', `\V'
;;           both within character class brackets and outside
;;   - word and non-word characters `\w' and `\W' (Emacs has the same
;;           syntax, but its meaning is different)
;;   - `s' (single line) and `x' (extended syntax) flags, in regexp
;;     literals, or set within the expression via `(?xs-xs)' or `(?xs-xs:
;;     .... )' syntax
;;   - comments `(?# ... )'

;;   Most of the more esoteric PCRE features can't really be supported by
;;   simple translation to Elisp regexps. These include the different
;;   lookaround assertions, conditionals, and the "backtracking control
;;   verbs" `(* ...)' . OTOH, there are a few other syntaxes which are
;;   currently unsupported and possibly could be:

;;   - `\L', `\U', `\l', `\u' case modifiers
;;   - `\g{...}' backreferences


;; 3.2 Other limitations
;; ~~~~~~~~~~~~~~~~~~~~~

;;   - The order of alternatives and characters in char classes sometimes
;;     gets shifted around, which is annoying.
;;   - Although the string parser tries to interpret PCRE's octal and
;;     hexadecimal escapes correctly, there are problems with matching
;;     8-bit characters that I don't use enough to properly understand,
;;     e.g.:
;;     ,----
;;     | (string-match-p (rxt-pcre-to-elisp "\\377") "\377") => nil
;;     `----
;;     A fix for this would be welcome.

;;   - Most of PCRE's rules for how `^', `\A', `$' and `\Z' interact with
;;     newlines are not implemented, since they seem less relevant to
;;     Emacs's buffer-oriented rather than line-oriented model.  However,
;;     the different meanings of the `.' metacharacter *are* implemented
;;     (it matches newlines with the `/s' flag, but not otherwise).

;;   - Not currently namespace clean (both `rxt-' and a couple of `pcre-'
;;     functions).


;; 3.3 TODO:
;; ~~~~~~~~~

;;   - Python-specific extensions to PCRE?
;;   - Language-specific stuff to enable regexp font-locking and explaining
;;     in different modes. Each language would need two functions, which
;;     could be kept in an alist:

;;     1. A function to read PCRE regexps, taking the string syntax into
;;        account. E.g., Python has single-quoted, double-quoted and raw
;;        strings, each with different quoting rules.  PHP has the kind of
;;        belt-and-suspenders solution you would expect: regexps are in
;;        strings, /and/ you have to include the `/ ...  /' delimiters!
;;        Duh.

;;     2. A function to copy faces back from the parsed string to the
;;        original buffer text. This has to recognize any escape sequences
;;        so they can be treated as a single character.


;; 4 Internal details
;; ==================

;;   Internally, `rxt' defines an abstract syntax tree data type for
;;   regular expressions, parsers for Elisp and PCRE syntax, and
;;   "unparsers" from to PCRE, rx, and SRE syntax. Converting from a parsed
;;   syntax tree to Elisp syntax is a two-step process: first convert to
;;   `rx' form, then let `rx-to-string' do the heavy lifting.  See
;;   `rxt-parse-re', `rxt-adt->pcre', `rxt-adt->rx', and `rxt-adt->sre',
;;   and the section beginning "Regexp ADT" in pcre2el.el for details.

;;   This code is partially based on Olin Shivers' reference SRE
;;   implementation in scsh, although it is simplified in some respects and
;;   extended in others. See `scsh/re.scm', `scsh/spencer.scm' and
;;   `scsh/posixstr.scm' in the `scsh' source tree for details. In
;;   particular, `pcre2el' steals the idea of an abstract data type for
;;   regular expressions and the general structure of the string regexp
;;   parser and unparser. The data types for character sets are extended in
;;   order to support symbolic translation between character set
;;   expressions without assuming a small (Latin1) character set. The
;;   string parser is also extended to parse a bigger variety of
;;   constructions, including POSIX character classes and various Emacs and
;;   Perl regexp assertions. Otherwise, only the bare minimum of scsh's
;;   abstract data type is implemented.


;; 5 Soapbox
;; =========

;;   Emacs regexps have their annoyances, but it is worth getting used to
;;   them. The Emacs assertions for word boundaries, symbol boundaries, and
;;   syntax classes depending on the syntax of the mode in effect are
;;   especially useful. (PCRE has `\b' for word-boundary, but AFAIK it
;;   doesn't have separate assertions for beginning-of-word and
;;   end-of-word). Other things that might be done with huge regexps in
;;   other languages can be expressed more understandably in Elisp using
;;   combinations of `save-excursion' with the various searches (regexp,
;;   literal, skip-syntax-forward, sexp-movement functions, etc.).

;;   There's not much point in using `rxt-pcre-to-elisp' to use PCRE
;;   notation in a Lisp program you're going to maintain, since you still
;;   have to double all the backslashes.  Better to just use the converted
;;   result (or better yet, the `rx' form).


;; 6 History and acknowledgments
;; =============================

;;   This was originally created out of an answer to a stackoverflow
;;   question:
;;   [http://stackoverflow.com/questions/9118183/elisp-mechanism-for-converting-pcre-regexps-to-emacs-regexps]

;;   Thanks to:

;;   - Wes Hardaker (hardaker) for the initial inspiration and subsequent
;;     hacking
;;   - priyadarshan for requesting RX/SRE support
;;   - Daniel Colascione (dcolascione) for a patch to support Emacs's
;;     explicitly-numbered match groups
;;   - Aaron Meurer (asmeurer) for requesting Isearch support
;;   - Philippe Vaucher (silex) for a patch to support `ibuffer-do-replace-regexp'
;;     in PCRE mode

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 're-builder)
(require 'advice)
(require 'ring)

;;; Customization group
(defgroup rxt nil
  "Regex syntax converter and utilities."
  :version 1.2
  :group 'tools
  :group 'lisp
  :link '(emacs-commentary-link :tag "commentary" "pcre2el.el")
  :link '(emacs-library-link :tag "lisp file" "pcre2el.el")
  :link '(url-link :tag "web page" "https://github.com/joddie/pcre2el"))

(defface rxt-highlight-face
  '((((min-colors 16581375) (background light)) :background "#eee8d5")
    (((min-colors 16581375) (background dark)) :background "#222222"))
  "Face for highlighting corresponding regex syntax in `rxt-explain' buffers."
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


;;;; Macros and functions for writing interactive input and output

;; Macro for returning values. If called interactively, display the
;; value in the echo area and copy it to the kill ring; otherwise just
;; return the value. TYPE controls what to copy to the kill ring. If
;; it is `pcre', copy the result as a string for yanking into Perl
;; code, JS code, etc. If `sexp', copy the result as a `read'-able
;; literal. If type is `emacs', copy both the string value (for use in
;; interactive commands) and a readable string literal (for yanking
;; into source buffers.
(defmacro rxt-value (type expr)
  (let ((val (make-symbol "val")))
    `(let ((,val ,expr))
       (when (called-interactively-p 'any)
         ,(cl-ecase type
            (sexp `(rxt--kill-sexp-value ,val))
            (pcre `(rxt--kill-pcre-value ,val))
            (emacs `(rxt--kill-lisp-value ,val))))
       ,val)))

(defun rxt--kill-sexp-value (value)
  (let ((lisp-literal (prin1-to-string value)))
    (message "Copied %s to kill-ring"
             (propertize lisp-literal 'face 'font-lock-string-face))
    (kill-new lisp-literal)))

(defun rxt--kill-pcre-value (value)
  (message "Copied PCRE %s to kill-ring"
           (propertize value 'face 'font-lock-string-face))
  (kill-new value))

(defun rxt--kill-lisp-value (value)
  (let ((lisp-literal (prin1-to-string value)))
    (message "Copied Emacs %s + string literal to kill-ring"
             (propertize value 'face 'font-lock-string-face))
    (kill-new lisp-literal)
    (kill-new value)))

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
                (save-excursion
                  (while (nth 3 (syntax-ppss)) (forward-char))
                  (let ((re (eval (preceding-sexp))))
                    (if (stringp re) re
                      (read-string prompt))))
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

;; Macro: interactively call one of two functions depending on the
;; major mode
(defmacro rxt-mode-dispatch (elisp-function pcre-function)
  `(if (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
       (call-interactively #',elisp-function)
     (call-interactively #',pcre-function)))


;;;; Minor mode for using emulated PCRE syntax

;;;###autoload
(define-minor-mode pcre-mode
  "Use emulated PCRE syntax for regexps wherever possible.

Advises the `interactive' specs of `read-regexp' and the
following other functions so that they read PCRE syntax and
translate to its Emacs equivalent:

- `align-regexp'
- `find-tag-regexp'
- `sort-regexp-fields'
- `isearch-message-prefix'
- `ibuffer-do-replace-regexp'

Also alters the behavior of `isearch-mode' when searching by regexp."
  nil " PCRE"
  nil
  :global t

  (if pcre-mode
      ;; Enabling
      (progn
        ;; Enable advice
        (ad-enable-regexp "pcre-mode")
        ;; Set up isearch hooks
        (add-hook 'isearch-mode-hook #'pcre-isearch-mode-hook)
        (add-hook 'isearch-mode-end-hook #'pcre-isearch-mode-end-hook))

    ;; Disable advice
    (ad-disable-regexp "pcre-mode")
    ;; Remove from isearch hooks
    (remove-hook 'isearch-mode-hook #'pcre-isearch-mode-hook)
    (remove-hook 'isearch-mode-end-hook #'pcre-isearch-mode-end-hook))

  ;; "Activating" advice re-computes the function definitions, which
  ;; is necessary whether enabling or disabling
  (ad-activate-regexp "pcre-mode"))

(defvar pcre-old-isearch-search-fun-function nil
  "Saved value of `isearch-search-fun-function' to restore on exiting `pcre-mode.'")
(make-variable-buffer-local 'pcre-old-isearch-search-fun-function)

;;; Cache of PCRE -> Elisp translations
(defvar pcre-mode-cache-size 100
  "Number of PCRE-to-Emacs translations to keep in the `pcre-mode' cache.")

(defvar pcre-mode-cache (make-hash-table :test 'equal)
  "Cache of PCRE-to-Emacs translations used in `pcre-mode'.

Keys are PCRE regexps, values are their Emacs equivalents.")

(defvar pcre-mode-reverse-cache (make-hash-table :test 'equal)
  "Cache of original PCREs translated to Emacs syntax in `pcre-mode'.

Keys are translated Emacs regexps, values are their original PCRE
form.  This is used to display the original PCRE regexp in place
of its translated form.")

(defvar pcre-cache-ring (make-ring pcre-mode-cache-size)
  "Ring of PCRE-to-Emacs translations used in `pcre-mode'.

When the ring fills up, the oldest element is removed and the
corresponding entries are deleted from the hash tables
`pcre-mode-cache' and `pcre-mode-reverse-cache'.")

(defun pcre-to-elisp/cached (pcre)
  "Translate PCRE to Emacs syntax, caching both forms."
  (or (gethash pcre pcre-mode-cache)
      (let ((elisp (rxt-pcre-to-elisp pcre)))
        (pcre-set-cache pcre elisp)
        elisp)))

(defun pcre-set-cache (pcre-regexp emacs-regexp)
  "Add a PCRE-to-Emacs translation to the `pcre-mode' cache."
  (when (and (not (zerop (length pcre-regexp)))
             (not (zerop (length emacs-regexp)))
             (not (gethash pcre-regexp pcre-mode-cache)))
    (if (= (ring-length pcre-cache-ring) (ring-size pcre-cache-ring))
        (let* ((old-item (ring-remove pcre-cache-ring))
               (old-pcre (car old-item))
               (old-emacs (cdr old-item)))
          (remhash old-pcre pcre-mode-cache)
          (remhash old-emacs pcre-mode-reverse-cache))
      (puthash pcre-regexp emacs-regexp pcre-mode-cache)
      (puthash emacs-regexp pcre-regexp pcre-mode-reverse-cache)
      (ring-insert pcre-cache-ring (cons pcre-regexp emacs-regexp)))))

;;; Isearch advice
(defun pcre-isearch-mode-hook ()
  (when (not (eq isearch-search-fun-function #'isearch-search-fun-default))
    (message "Warning: pcre-mode overriding existing isearch function `%s'"
             isearch-search-fun-function))
  (setq pcre-old-isearch-search-fun-function isearch-search-fun-function)
  (set (make-local-variable 'isearch-search-fun-function)
       #'pcre-isearch-search-fun-function))

(defun pcre-isearch-mode-end-hook ()
  (setq isearch-search-fun-function pcre-old-isearch-search-fun-function))

(defun pcre-isearch-search-fun-function ()
  "Enable isearching using emulated PCRE syntax.

This is set as the value of `isearch-search-fun-function' when
`pcre-mode' is enabled.  Returns a function which searches using
emulated PCRE regexps when `isearch-regexp' is true."
  (if (not isearch-regexp)
      (isearch-search-fun-default)
    (lambda (string bound noerror)
      ;; Raise an error if the regexp ends in an incomplete escape
      ;; sequence (= odd number of backslashes).
      ;; TODO: Perhaps this should really be handled in rxt-pcre-to-elisp?
      (if (isearch-backslash string) (rxt-error "Trailing backslash"))
      (let ((regexp (pcre-to-elisp/cached string)))
        (if isearch-forward
            (re-search-forward regexp bound noerror)
          (re-search-backward regexp bound noerror))))))

(defadvice isearch-message-prefix (after pcre-mode disable)
  "Add \"PCRE\" to the Isearch message when searching by regexp in `pcre-mode'."
  (when (and isearch-regexp
             ;; Prevent an inaccurate message if our callback was
             ;; removed somehow
             (eq isearch-search-fun-function #'pcre-isearch-search-fun-function))
    (let ((message ad-return-value))
      ;; Some hackery to give replacement the same fontification as
      ;; the original
      (when
          (let ((case-fold-search t)) (string-match "regexp" message))
        (let* ((match (match-string 0 message))
               (properties (text-properties-at 0 match))
               (replacement (apply #'propertize "PCRE regexp" properties))
               (new-message (replace-match replacement t t message)))
          (setq ad-return-value new-message))))))

(defadvice isearch-fallback
  (before pcre-mode (want-backslash &optional allow-invalid to-barrier) disable)
  "Hack to fall back correctly in `pcre-mode'. "
  ;; A dirty hack to the internals of isearch.  Falling back to a
  ;; previous match position is necessary when the (Emacs) regexp ends
  ;; in "*", "?", "\{" or "\|": this is handled in
  ;; `isearch-process-search-char' by calling `isearch-fallback' with
  ;; `t' for the value of the first parameter, `want-backslash', in
  ;; the last two cases.  With PCRE regexps, falling back should take
  ;; place on "*", "?", "{" or "|", with no backslashes required.
  ;; This advice handles the last two cases by unconditionally setting
  ;; `want-backslash' to nil.
  (ad-set-arg 0 nil))

;;; Other hooks and defadvices

;;;###autoload
(defun pcre-query-replace-regexp ()
  "Perform `query-replace-regexp' using PCRE syntax.

Consider using `pcre-mode' instead of this function."
  (interactive)
  (let ((old-pcre-mode pcre-mode))
    (unwind-protect
        (progn
          (pcre-mode +1)
          (call-interactively #'query-replace-regexp))
      (pcre-mode (if old-pcre-mode 1 0)))))


(defadvice add-to-history
  (before pcre-mode (history-var newelt &optional maxelt keep-all) disable)
  "Add the original PCRE to query-replace history in `pcre-mode'."
  (when (eq history-var query-replace-from-history-variable)
    (let ((original (gethash newelt pcre-mode-reverse-cache)))
      (when original
        (ad-set-arg 1 original)))))

(defadvice query-replace-descr
  (before pcre-mode (from) disable)
  "Use the original PCRE in Isearch prompts in `pcre-mode'."
  (let ((original (gethash from pcre-mode-reverse-cache)))
    (when original
      (ad-set-arg 0 original))))

;;; The `interactive' specs of the following functions are lifted
;;; wholesale from the original built-ins, which see.
(defadvice read-regexp
  (around pcre-mode first (prompt &optional defaults history) disable)
  "Read regexp using PCRE syntax and convert to Elisp equivalent."
  (ad-set-arg 0 (concat "[PCRE] " prompt))
  ad-do-it
  (setq ad-return-value
        (pcre-to-elisp/cached ad-return-value)))

(defadvice align-regexp
  (before pcre-mode first (beg end regexp &optional group spacing repeat) disable)
  "Read regexp using PCRE syntax and convert to Elisp equivalent."
  (interactive
   (append
    (list (region-beginning) (region-end))
    (if current-prefix-arg
        (list (rxt-pcre-to-elisp
               (read-string "Complex align using PCRE regexp: "
                            "(\\s*)"))
              (string-to-number
               (read-string
                "Parenthesis group to modify (justify if negative): " "1"))
              (string-to-number
               (read-string "Amount of spacing (or column if negative): "
                            (number-to-string align-default-spacing)))
              (y-or-n-p "Repeat throughout line? "))
      (list (concat "\\(\\s-*\\)"
                    (rxt-pcre-to-elisp
                     (read-string "Align PCRE regexp: ")))
            1 align-default-spacing nil)))))

(defadvice ibuffer-do-replace-regexp
  (before pcre-mode first (from-str to-str) disable)
  "Read regexp using PCRE syntax and convert to Elisp equivalent."
  (interactive
   (let* ((from-str (read-from-minibuffer "[PCRE] Replace regexp: "))
          (to-str (read-from-minibuffer (concat "[PCRE] Replace " from-str " with: "))))
     (list (rxt-pcre-to-elisp from-str) to-str))))

(defadvice find-tag-regexp
  (before pcre-mode first (regexp &optional next-p other-window) disable)
  "Read regexp using PCRE syntax and convert to Elisp equivalent."
  "Perform `find-tag-regexp' using emulated PCRE regexp syntax."
  (interactive
   (let ((args (find-tag-interactive "[PCRE] Find tag regexp: " t)))
     (list (rxt-pcre-to-elisp (nth 0 args))
           (nth 1 args) (nth 2 args)))))

(defadvice sort-regexp-fields
  (before pcre-mode first (reverse record-regexp key-regexp beg end) disable)
  "Read regexp using PCRE syntax and convert to Elisp equivalent."
  (interactive "P\nsPCRE regexp specifying records to sort: \n\
sPCRE regexp specifying key within record: \nr")
  (ad-set-arg 1 (rxt-pcre-to-elisp (ad-get-arg 1)))
  (ad-set-arg 2 (rxt-pcre-to-elisp (ad-get-arg 2))))



;;; Commands that translate Elisp to other formats

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

;;;###autoload
(defun rxt-toggle-elisp-rx ()
  "Toggle the regexp near point between Elisp string and rx syntax."
  (interactive)
  ;; First, position point before the regex form near point (either
  ;; a string literal or a list beginning `rx' or `rx-to-string').
  (let* ((context (syntax-ppss))
         (string-start (nth 8 context)))
    (cond (string-start (goto-char string-start))
          ((looking-back "\"") (backward-sexp))
          ((looking-at "\"") nil)
          (t
           ;; Search backwards, leaving point in place on error
           (goto-char
            (save-excursion
              (skip-syntax-forward "-")
              (while (not (looking-at
                           (rx "(" (or "rx" "rx-to-string") symbol-end)))
                (backward-up-list))
              (point))))))

  ;; Read and replace the regex following point
  (let* ((regex (read (current-buffer)))
         (print-escape-newlines t))
    (save-excursion
      (if (listp regex)
          ;; Replace rx form with string value
          (prin1 (eval regex) (current-buffer))
        ;; Pretty-print rx form
        (save-restriction
          (let* ((start (point))
                 (rx-syntax (rxt-elisp-to-rx regex))
                 (rx-form
                  (pcase rx-syntax
                    (`(seq . ,rest) `(rx . ,rest))
                    (form           `(rx ,form)))))
            (prin1 rx-form (current-buffer))
            (narrow-to-region start (point)))
          (pp-buffer)
          ;; remove the extra newline that pp-buffer inserts
          (goto-char (point-max))
          (delete-region
           (point)
           (save-excursion (skip-chars-backward " \t\n") (point))))))
    (kill-sexp -1)
    (indent-pp-sexp)))



;;; Commands that translate PCRE to other formats

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


;;; Regexp explaining functions to display pretty-printed rx syntax

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
    (cl-destructuring-bind (ast fontified)
        (rxt-parse-and-fontify regexp)
      (rxt-pp-rx fontified (rxt-adt->rx ast)))))

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


;;;; Commands that depend on the major mode in effect

;;;###autoload
(defun rxt-explain ()
  "Pop up a buffer with pretty-printed `rx' syntax for the regex at point.

Chooses regex syntax to read based on current major mode, calling
`rxt-explain-elisp' if buffer is in `emacs-lisp-mode' or
`lisp-interaction-mode', or `rxt-explain-pcre' otherwise."
  (interactive)
  (rxt-mode-dispatch rxt-explain-elisp rxt-explain-pcre))

;;;###autoload
(defun rxt-convert-syntax ()
  "Convert regex at point to other kind of syntax, depending on major mode.

For buffers in `emacs-lisp-mode' or `lisp-interaction-mode',
calls `rxt-elisp-to-pcre' to convert to PCRE syntax. Otherwise,
calls `rxt-pcre-to-elisp' to convert to Emacs syntax.

The converted syntax is displayed in the echo area and copied to
the kill ring; see the two functions named above for details."
  (interactive)
  (rxt-mode-dispatch rxt-elisp-to-pcre rxt-pcre-to-elisp))

;;;###autoload
(defun rxt-convert-to-rx ()
  "Convert regex at point to RX syntax. Chooses Emacs or PCRE syntax by major mode."
  (interactive)
  (rxt-mode-dispatch rxt-elisp-to-rx rxt-pcre-to-rx))

;;;###autoload
(defun rxt-convert-to-strings ()
  "Convert regex at point to RX syntax. Chooses Emacs or PCRE syntax by major mode."
  (interactive)
  (rxt-mode-dispatch rxt-elisp-to-strings rxt-pcre-to-strings))



;;; Minor mode and keybindings

(defvar rxt-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Generic
    (define-key map (kbd "C-c / /") 'rxt-explain)
    (define-key map (kbd "C-c / c") 'rxt-convert-syntax)
    (define-key map (kbd "C-c / x") 'rxt-convert-to-rx)
    (define-key map (kbd "C-c / '") 'rxt-convert-to-strings)

    ;; From PCRE
    (define-key map (kbd "C-c / p /") 'rxt-explain-pcre)
    (define-key map (kbd "C-c / p e") 'rxt-pcre-to-elisp)
    (define-key map (kbd "C-c / p x") 'rxt-pcre-to-rx)
    (define-key map (kbd "C-c / p s") 'rxt-pcre-to-sre)
    (define-key map (kbd "C-c / p '") 'rxt-pcre-to-strings)

    ;; From Elisp
    (define-key map (kbd "C-c / e /") 'rxt-explain-elisp)
    (define-key map (kbd "C-c / e p") 'rxt-elisp-to-pcre)
    (define-key map (kbd "C-c / e x") 'rxt-elisp-to-rx)
    (define-key map (kbd "C-c / e s") 'rxt-elisp-to-sre)
    (define-key map (kbd "C-c / e '") 'rxt-elisp-to-strings)
    (define-key map (kbd "C-c / e t") 'rxt-toggle-elisp-rx)
    (define-key map (kbd "C-c / t") 'rxt-toggle-elisp-rx)
    (define-key map (kbd "C-c / h") 'rxt-fontify-regexp-at-point)

    ;; Search
    (define-key map (kbd "C-c / %") 'pcre-query-replace-regexp)

    map)
  "Keymap for `rxt-mode'.")

;;;###autoload
(define-minor-mode rxt-mode
  "Regex translation utilities." nil nil)

;;;###autoload
(defun turn-on-rxt-mode ()
  "Turn on `rxt-mode' in the current buffer."
  (interactive)
  (rxt-mode 1))

;;;###autoload
(define-globalized-minor-mode rxt-global-mode rxt-mode
  turn-on-rxt-mode)


;;;; Syntax explanations

;; Major mode for displaying pretty-printed S-exp syntax
(define-derived-mode rxt-help-mode emacs-lisp-mode "Regexp Explain"
  (setq buffer-read-only t)
  (add-hook 'post-command-hook 'rxt-highlight-text nil t)
  (rxt-highlight-text))

;; Hack: stop paredit-mode interfering with `rxt-print-rx'
(eval-when-compile (declare-function paredit-mode "paredit.el"))
(add-hook 'rxt-help-mode-hook
          (lambda ()
            (if (and (boundp 'paredit-mode)
                     paredit-mode)
                (paredit-mode 0))))

(define-key rxt-help-mode-map "q" 'quit-window)
(define-key rxt-help-mode-map "z" 'kill-this-buffer)
(define-key rxt-help-mode-map "n" 'next-line)
(define-key rxt-help-mode-map "p" 'previous-line)
(define-key rxt-help-mode-map "f" 'forward-list)
(define-key rxt-help-mode-map "b" 'backward-list)
(define-key rxt-help-mode-map "u" 'backward-up-list)
(define-key rxt-help-mode-map "d" 'down-list)

(defun rxt-pp-rx (regexp rx)
  "Display string regexp REGEXP with its `rx' form RX in an `rxt-help-mode' buffer."
  (with-current-buffer (get-buffer-create "* Regexp Explain *")
    (let ((print-escape-newlines t)
          (inhibit-read-only t))
      (erase-buffer)
      (rxt-help-mode)
      (rxt--insert-displaying-escapes regexp)
      (newline 2)
      (save-excursion
        (let ((sexp-begin (point)))
          (rxt-print-rx rx)
          (narrow-to-region sexp-begin (point))
          (pp-buffer)
          (widen)))
      (rxt-highlight-text))
    (pop-to-buffer (current-buffer))))

(defun rxt--display-character-as (begin end char display)
  (save-excursion
    (goto-char begin)
    (while (search-forward char end t)
      (let ((ol (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ol 'display display)))))

(defun rxt--insert-displaying-escapes (str)
  (let ((begin (point)))
    (insert str)
    (rxt--display-character-as begin (point) "\n" "\\n")
    (rxt--display-character-as begin (point) "\t" "\\t")
    (rxt--display-character-as begin (point) "\f" "\\f")
    (rxt--display-character-as begin (point) "\r" "\\r")))

(cl-defun rxt-print-rx (rx &optional (depth 0))
  "Print RX like `print', adding text overlays for corresponding source locations."
  (let ((re (gethash rx rxt-explain-hash-map))
        (begin (point)))
    (cl-typecase rx
      (cons
       (insert "(")
       (cl-loop for tail on rx
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
      (string
       (rxt--insert-displaying-escapes (prin1-to-string rx)))
      (t
       (prin1 rx (current-buffer))))
    (when (and re
               (rxt-syntax-tree-begin re)
               (rxt-syntax-tree-end re))
      (let* ((sexp-begin (copy-marker begin t))
             (sexp-end (copy-marker (point)))
             (sexp-bounds (list sexp-begin sexp-end))
             (source-begin (1+ (rxt-syntax-tree-begin re)))
             (source-end (1+ (rxt-syntax-tree-end re)))
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
      (cl-destructuring-bind (begin end) bounds
        (let ((overlay (make-overlay begin end)))
          (push overlay rxt-highlight-overlays)
          (overlay-put overlay 'face 'rxt-highlight-face))))))


;;;; Error handling

(put 'rxt-invalid-regexp
     'error-conditions
     '(error invalid-regexp rxt-invalid-regexp))

(defun rxt-error (&rest format-args)
  (signal 'rxt-invalid-regexp (list (apply #'format format-args))))


;;;; Regexp syntax tree data type

;; Base class that keeps the source text as a string with offsets
;; beginning and ending parsed portion
(cl-defstruct
    rxt-syntax-tree
  begin end source)

(defun rxt-syntax-tree-readable (tree)
  (cl-assert (rxt-syntax-tree-p tree))
  (let ((begin (rxt-syntax-tree-begin tree))
        (end (rxt-syntax-tree-end tree))
        (source (rxt-syntax-tree-source tree)))
    (if (and begin end source)
        (substring source begin end)
      (let ((print-level 1))
        (prin1-to-string tree)))))


;; Literal string
(cl-defstruct
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
           (rxt-string (cl-reduce #'concat strs
                                  :key #'rxt-string-chars))))
      (setf (rxt-syntax-tree-begin result) (rxt-syntax-tree-begin (car strs))
            (rxt-syntax-tree-end result) (rxt-syntax-tree-end (car (last strs))))
      result)))

;;; Other primitives
(cl-defstruct (rxt-primitive
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
(cl-defstruct
    (rxt-seq
     (:constructor make-rxt-seq (elts))
     (:include rxt-syntax-tree))
  elts)

;; Slightly smart sequence constructor:
;; - Flattens nested sequences
;; - Drops trivial "" elements
;; - Empty sequence => ""
;; - Singleton sequence is reduced to its one element.
(defun rxt-seq (res)        ; Flatten nested seqs & drop ""'s.
  (let ((res (rxt-seq-flatten res)))
    (if (consp res)
        (if (consp (cdr res))
            (make-rxt-seq res) ; General case
          (car res))           ; Singleton sequence
      rxt-empty-string)))      ; Empty seq -- ""

(defun rxt-seq-flatten (res)
  (if (consp res)
      (let ((re (car res))
            (tail (rxt-seq-flatten (cdr res))))
        (cond ((rxt-seq-p re)           ; Flatten nested seqs
               (append (rxt-seq-flatten (rxt-seq-elts re)) tail))
              ((rxt-trivial-p re) tail) ; Drop trivial elts
              ((and (rxt-string-p re)   ; Flatten strings
                    (consp tail)
                    (rxt-string-p (car tail)))
               (cons
                (rxt-string-concat re (car tail))
                (cdr tail)))
              (t (cons re tail))))
    '()))

;;; Choice
(cl-defstruct
    (rxt-choice
     (:constructor make-rxt-choice (elts))
     (:include rxt-syntax-tree))
  elts)

;;; The empty choice represents a regexp that never matches in any context
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
            (make-rxt-choice res) ; General case
          (car res))              ; Singleton choice
      rxt-empty)))

;; Flatten any nested rxt-choices amongst RES, and collect any
;; charsets together
(defun rxt-choice-flatten (res)
  (cl-destructuring-bind (res cset)
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
      (list '() (make-rxt-char-set-union))
    (let* ((re (car res)))
      (cl-destructuring-bind (tail cset)
          (rxt-choice-flatten+char-set (cdr res))
        (cond ((rxt-choice-p re)         ; Flatten nested choices
               (list
                (append (rxt-choice-elts re) tail)
                cset))

              ((rxt-empty-p re)          ; Drop empty re's.
               (list tail cset))

              ((rxt-char-set-union-p re) ; Fold char sets together
               (list tail
                     (rxt-char-set-adjoin! cset re)))

              ((and (rxt-string-p re)    ; Same for 1-char strings
                    (= 1 (length (rxt-string-chars re))))
               (list tail
                     (rxt-char-set-adjoin! cset
                                           (rxt-string-chars re))))

              (t                         ; Otherwise.
               (list (cons re tail) cset)))))))

;;; Repetition
(cl-defstruct (rxt-repeat
               (:include rxt-syntax-tree))
  from to body greedy)

(cl-defun rxt-repeat (from to body &optional (greedy t))
  (if (equal to 0)
      rxt-empty-string
    (make-rxt-repeat :from from :to to
                     :body body :greedy greedy)))

;;; Submatch
(cl-defstruct
    (rxt-submatch
     (:constructor rxt-submatch (body))
     (:include rxt-syntax-tree))
  body)

;;; Numbered submatch (Emacs only)
(cl-defstruct
    (rxt-submatch-numbered
     (:constructor rxt-submatch-numbered (n body))
     (:include rxt-syntax-tree))
  n
  body)

;;; Backreference
(cl-defstruct
    (rxt-backref
     (:constructor rxt-backref (n))
     (:include rxt-syntax-tree))
  n)

;;; Syntax classes (Emacs only)
(cl-defstruct (rxt-syntax-class
               (:include rxt-syntax-tree))
  symbol)

(defun rxt-syntax-class (symbol)
  (if (assoc symbol rx-syntax)
      (make-rxt-syntax-class :symbol symbol)
    (rxt-error "Invalid syntax class symbol %s" symbol)))

;;; Character categories (Emacs only)
(cl-defstruct (rxt-char-category
               (:include rxt-syntax-tree))
  symbol)

(defun rxt-char-category (symbol)
  (if (assoc symbol rx-categories)
      (make-rxt-char-category :symbol symbol)
    (rxt-error "Invalid character category symbol %s" symbol)))


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
(cl-defstruct (rxt-char-set-union
               (:include rxt-syntax-tree))
  chars    ; list of single characters
  ranges   ; list of ranges (from . to)
  classes) ; list of character classes

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
    (rxt-error "Can't construct character set union from %S" item))))

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
  (cl-assert (rxt-char-set-union-p cset))

  (cond
   ((integerp item)     ; character
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

   ((symbolp item)      ; posix character class
    (push item (rxt-char-set-union-classes cset)))

   ((rxt-char-set-union-p item)
    (dolist (type (list (rxt-char-set-union-chars item)
                        (rxt-char-set-union-ranges item)
                        (rxt-char-set-union-classes item)))
      (dolist (thing type)
        (rxt-char-set-adjoin! cset thing))))

   (t
    (rxt-error "Can't adjoin non-rxt-char-set, character, range or symbol %S" item)))
  cset)


;;; Set complement of character set, syntax class, or character
;;; category

;; In general, all character sets that can be represented in string
;; notation as [^ ... ] (but see `rxt-char-set-intersection', below), plus
;; Emacs' \Sx and \Cx constructions.
(cl-defstruct (rxt-char-set-negation
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
         (rxt-error "Can't negate non-char-set or syntax class %s" cset))))

;;; Intersections of char sets

;; These are difficult to represent in general, but can be constructed
;; in Perl using double negation; for example: [^\Wabc] means the set
;; complement of [abc] with respect to the universe of "word
;; characters": (& (~ (~ word)) (~ ("abc"))) == (& word (~ ("abc")))
;; == (- word ("abc"))

(cl-defstruct (rxt-char-set-intersection
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
        (rxt-error "Can't take intersection of non-character-set %s" cset))))

    (if (null elts)
        (rxt-negate cmpl)
      (unless (rxt-empty-char-set-p cmpl)
        (push (rxt-negate cmpl) elts))
      (if (null (cdr elts))
          (car elts)      ; singleton case
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



;;;; Macros for building the parser

(eval-when-compile
  (defmacro rxt-token-case (&rest cases)
    "Consume a token at point and evaluate corresponding forms.

CASES is a list of `cond'-like clauses, (REGEXP BODY ...) where
the REGEXPs define possible tokens which may appear at point. The
CASES are considered in order. For each case, if the text at
point matches REGEXP, then point is moved to the end of the
matched token, the corresponding BODY is evaluated and their
value returned. The matched token is available within the BODY
forms as (match-string 0).

There can be a default case where REGEXP is `t', which evaluates
the corresponding FORMS but does not move point.

Returns `nil' if none of the CASES matches."
    (declare (debug (&rest (sexp &rest form))))
    `(cond
      ,@(cl-loop for (token . action) in cases
                 collect
                 (if (eq token t)
                     `(t ,@action)
                   `((looking-at ,token)
                     (goto-char (match-end 0))
                     ,@action)))))

  (defmacro rxt-fontify-token-case (&rest cases)
    "Consume and font-lock a token at point, and evaluate corresponding forms.

CASES is a list of clauses (REGEXP BODY ...), as for
`rxt-token-case'. If one of the REGEXPs matches text at point,
the corresponding BODY forms are evaluated, and should return a
cons of the form (FONT . RESULT). FONT is applied as to the
matching token as a `font-lock-face' property and the value of
RESULT is returned."
    (declare (debug (&rest (sexp &rest form))))
    (let ((font (make-symbol "font"))
          (result (make-symbol "result")))
      `(rxt-token-case
        ,@(cl-loop for (token . action) in cases
                   collect
                   `(,token
                     (cl-destructuring-bind (,font . ,result)
                         (save-match-data ,@action)
                       (when ,font
                         (put-text-property
                          (match-beginning 0) (match-end 0)
                          'font-lock-face ,font))
                       ,result))))))

  (defmacro rxt-syntax-tree-value (&rest body)
    "Evaluate BODY, annotating its return value with source position information.

BODY must return an `rxt-syntax-tree' object. The value of
`rxt-syntax-tree-value' is a modified `rxt-syntax-tree' object
whose `rxt-syntax-tree-source' property is the string currently
being parsed, and whose `rxt-syntax-tree-begin' and
`rxt-syntax-tree-end' properties are the positions of point
before and after evaluating BODY, respectively."
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
         ,value))))

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
              (list pcre (match-string-no-properties 0))))))))))


;;;; Parser constants

(defconst rxt-pcre-word-chars
  (make-rxt-char-set-union :chars '(?_) :classes '(alnum)))
(defconst rxt-pcre-non-word-chars (rxt-negate rxt-pcre-word-chars))

(defconst rxt-digit-chars (rxt-simple-char-set 'digit))
(defconst rxt-non-digit-chars (rxt-negate rxt-digit-chars))
(defconst rxt-space-chars (rxt-simple-char-set 'space))
(defconst rxt-non-space-chars (rxt-negate rxt-space-chars))

(defconst rxt-pcre-horizontal-whitespace-chars
  (rxt-simple-char-set
   '(#x0009 #x0020 #x00A0 #x1680 #x180E #x2000 #x2001 #x2002 #x2003
            #x2004 #x2005 #x2006 #x2007 #x2008 #x2009 #x200A #x202F
            #x205F #x3000)))
(defconst rxt-pcre-non-horizontal-whitespace-chars
  (rxt-negate rxt-pcre-horizontal-whitespace-chars))

(defconst rxt-pcre-vertical-whitespace-chars
  (rxt-simple-char-set '(#x000A #x000B #x000C #x000D #x0085 #x2028 #x2029)))
(defconst rxt-pcre-non-vertical-whitespace-chars
  (rxt-negate rxt-pcre-vertical-whitespace-chars))

(defconst rxt-pcre-whitespace-chars
  (rxt-simple-char-set '(9 10 12 13 32)))
(defconst rxt-pcre-non-whitespace-chars
  (rxt-negate rxt-pcre-whitespace-chars))

(defconst rxt-pcre-named-classes-regexp
  (rx "[:"
      (submatch
       (or "alnum" "alpha" "ascii" "blank" "cntrl" "digit" "graph" "lower"
           "print" "punct" "space" "upper" "word" "xdigit"))
      ":]"))

(defconst rxt-elisp-named-classes-regexp
  (rx "[:"
      (submatch
       (or "alnum" "alpha" "ascii" "blank" "cntrl" "digit" "graph" "lower"
           "print" "punct" "space" "upper" "word" "xdigit"
           "unibyte" "nonascii" "multibyte"))
      ":]"))


;;;; Elisp and PCRE string notation parser


;;; The following dynamically bound variables control the operation of
;;; the parser (see `rxt-parse-and-fontify'.)

(defvar rxt-parse-pcre nil
  "t if the rxt string parser is parsing PCRE syntax, nil for Elisp syntax.

This should only be let-bound internally, never set otherwise.")

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

(defvar rxt-branch-end-regexp nil)
(defvar rxt-choice-regexp nil)
(defvar rxt-brace-begin-regexp nil)
(defvar rxt-m-to-n-brace-regexp nil)
(defvar rxt-m-to-?-brace-regexp nil)
(defvar rxt-m-brace-regexp nil)
(defvar rxt-named-classes-regexp nil)

(defvar rxt-subgroup-count nil)
(defvar rxt-source-text-string nil)

(defun rxt-parse-re (re &optional pcre flags)
  (cl-destructuring-bind (ast fontified)
      (rxt-parse-and-fontify re pcre flags)
    ast))

(defun rxt-parse-and-fontify (re &optional pcre flags)
  (let* ((rxt-parse-pcre pcre)
         (rxt-pcre-extended-mode
          (and pcre (stringp flags) (rxt-extended-flag-p flags)))
         (rxt-pcre-s-mode
          (and pcre (stringp flags) (rxt-s-flag-p flags)))

         ;; Bind regexps to match syntax that differs between PCRE and
         ;; Elisp only in the addition of a backslash "\"
         (escape (if pcre "" "\\"))
         (rxt-choice-regexp
          (rx-to-string `(seq ,escape "|")))
         (rxt-branch-end-regexp
          (rx-to-string `(or buffer-end
                             (seq ,escape (or "|" ")")))))
         (rxt-brace-begin-regexp
          (rx-to-string `(seq ,escape "{")))
         (rxt-m-to-n-brace-regexp
          (rx-to-string
           `(seq
             (submatch (* (any "0-9"))) "," (submatch (+ (any "0-9")))
             ,escape "}")))
         (rxt-m-to-?-brace-regexp
          (rx-to-string
           `(seq (submatch (+ (any "0-9"))) "," ,escape "}")))
         (rxt-m-brace-regexp
          (rx-to-string
           `(seq (submatch (+ (any "0-9"))) ,escape "}")))

         ;; Named character classes [: ... :] differ slightly
         (rxt-named-classes-regexp
          (if pcre
              rxt-pcre-named-classes-regexp
            rxt-elisp-named-classes-regexp))

         (rxt-subgroup-count 0)
         (case-fold-search nil))
    (with-temp-buffer
      (insert re)
      (goto-char (point-min))
      (let ((rxt-source-text-string re))
        (list (rxt-parse-exp)
              (buffer-string))))))

;; Parse a complete regex: a number of branches separated by | or
;; \|, as determined by `rxt-branch-end-regexp'.
(defun rxt-parse-exp ()
  ;; These variables are let-bound here because in PCRE mode they may
  ;; be set internally by (?x) or (?s) constructions, whose scope
  ;; lasts until the end of a sub-expression
  (rxt-syntax-tree-value
   (let ((rxt-pcre-extended-mode rxt-pcre-extended-mode)
         (rxt-pcre-s-mode rxt-pcre-s-mode))
     (if (eobp)
         (rxt-seq nil)
       (let ((branches '()))
         (cl-block nil
           (while t
             (let ((branch (rxt-parse-branch)))
               (push branch branches)
               (rxt-fontify-token-case
                (rxt-choice-regexp (cons 'font-lock-builtin-face nil))
                (t (cl-return (rxt-choice (reverse branches)))))))))))))

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
   (let ((pieces '())
         (branch-start-p t))
     (while (not (looking-at rxt-branch-end-regexp))
       (push (rxt-parse-piece branch-start-p) pieces)
       (setq branch-start-p nil))
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
  (rxt-fontify-token-case
   ((rx "*?") (cons 'font-lock-keyword-face (rxt-repeat 0 nil atom nil)))
   ((rx "*")  (cons 'font-lock-keyword-face (rxt-repeat 0 nil atom t)))
   ((rx "+?") (cons 'font-lock-keyword-face (rxt-repeat 1 nil atom nil)))
   ((rx "+")  (cons 'font-lock-keyword-face (rxt-repeat 1 nil atom t)))
   ((rx "??") (cons 'font-lock-keyword-face (rxt-repeat 0 1 atom nil)))
   ((rx "?")  (cons 'font-lock-keyword-face (rxt-repeat 0 1 atom t)))
   ;; Brace expression "{M,N}", "{M,}", "{M}"
   (rxt-brace-begin-regexp
    (cons 'font-lock-keyword-face
          (cl-destructuring-bind (from to)
              (rxt-parse-braces)
            (rxt-repeat from to atom))))
   ;; No quantifiers found
   (t (cons nil atom))))

;; Parse a regexp atom, i.e. an element that binds to any following
;; quantifiers. This includes characters, character classes,
;; parenthesized groups, assertions, etc.
(defun rxt-parse-atom (&optional branch-begin)
  (if (eobp)
      (rxt-error "Unexpected end of regular expression")
    (if rxt-parse-pcre
        (rxt-parse-atom/pcre)
      (rxt-parse-atom/el branch-begin))))

(defun rxt-parse-atom/common ()
  (rxt-fontify-token-case
   ((rx "[")   (cons 'font-lock-builtin-face (rxt-parse-char-class)))
   ((rx "\\b") (cons 'font-lock-constant-face rxt-word-boundary))
   ((rx "\\B") (cons 'font-lock-constant-face rxt-not-word-boundary))))

(defun rxt-parse-atom/el (branch-begin)
  (rxt-syntax-tree-value
   (or (rxt-parse-atom/common)
       (rxt-fontify-token-case
        ;; "." wildcard
        ((rx ".") (cons 'font-lock-variable-name-face rxt-nonl))
        ;; "^" and "$" are metacharacters only at beginning or end of a
        ;; branch in Elisp; elsewhere they are literals
        ((rx "^")
         (if branch-begin
             (cons 'font-lock-constant-face rxt-bol)
           (cons nil (rxt-string "^"))))
        ((rx "$")
         (if (looking-at rxt-branch-end-regexp)
             (cons 'font-lock-constant-face rxt-eol)
           (cons nil (rxt-string "$"))))
        ;; Beginning & end of string, word, symbol
        ((rx "\\`") (cons 'font-lock-constant-face rxt-bos))
        ((rx "\\'") (cons 'font-lock-constant-face rxt-eos))
        ((rx "\\<") (cons 'font-lock-constant-face rxt-bow))
        ((rx "\\>") (cons 'font-lock-constant-face rxt-eow))
        ((rx "\\_<") (cons 'font-lock-constant-face rxt-symbol-start))
        ((rx "\\_>") (cons 'font-lock-constant-face rxt-symbol-end))
        ;; Subgroup
        ((rx "\\(") (cons 'font-lock-builtin-face (rxt-parse-subgroup/el)))
        ;; Word/non-word characters (meaning depending on syntax table)
        ((rx "\\w") (cons 'font-lock-constant-face rxt-wordchar))
        ((rx "\\W") (cons 'font-lock-constant-face rxt-not-wordchar))
        ;; Other syntax categories
        ((rx "\\"
             (submatch (any "Ss"))
             (submatch (any "-.w_()'\"$\\/<>|!")))
         (cons 'font-lock-constant-face
               (let ((negated (string= (match-string 1) "S"))
                     (re
                      (rxt-syntax-class
                       (car (rassoc (string-to-char (match-string 2))
                                    rx-syntax)))))
                 (if negated (rxt-negate re) re))))
        ;; Character categories
        ((rx "\\"
             (submatch (any "Cc"))
             (submatch nonl))
         (cons 'font-lock-constant-face
               (let ((negated (string= (match-string 1) "C"))
                     (category
                      (car (rassoc (string-to-char (match-string 2))
                                   rx-categories))))
                 (unless category
                   (rxt-error "Unrecognized character category %s" (match-string 2)))
                 (let ((re (rxt-char-category category)))
                   (if negated (rxt-negate re) re)))))
        ;; Backreference
        ((rx (seq "\\" (submatch (any "1-9"))))
         (cons 'font-lock-variable-name-face
               (rxt-backref (string-to-number (match-string 1)))))
        ;; Other escaped characters
        ((rx (seq "\\" (submatch nonl)))
         (cons nil (rxt-string (match-string 1))))
        ;; Normal characters
        ((rx (or "\n" nonl))
         (cons nil (rxt-string (match-string 0))))))))

(defun rxt-parse-atom/pcre ()
  (rxt-extended-skip)
  (rxt-syntax-tree-value
   (or
    ;; Is it an atom that's the same in Elisp?
    (rxt-parse-atom/common)
    ;; Is it common to PCRE regex and character class syntax?
    (let ((char (rxt-parse-escapes/pcre)))
      (and char
           (rxt-string (char-to-string char))))
    ;; Otherwise:
    (rxt-fontify-token-case
     ;; "." wildcard
     ((rx ".")
      (cons 'font-lock-variable-name-face
            (if rxt-pcre-s-mode
                rxt-anything
              rxt-nonl)))
     ;; Beginning & end of string/line
     ((rx "^") (cons 'font-lock-constant-face rxt-bol))
     ((rx "$") (cons 'font-lock-constant-face rxt-eol))
     ((rx "\\A") (cons 'font-lock-constant-face rxt-bos))
     ((rx "\\Z") (cons 'font-lock-constant-face rxt-eos))
     ;; Subgroup
     ((rx "(") (cons 'font-lock-builtin-face (rxt-parse-subgroup/pcre)))
     ;; Metacharacter quoting
     ((rx "\\Q")
      ;; It would seem simple to take all the characters between \Q
      ;; and \E and make an rxt-string, but \Q...\E isn't an atom:
      ;; any quantifiers afterward should bind only to the last
      ;; character, not the whole string.
      (cons 'font-lock-builtin-face
            (let ((begin (point)))
              (search-forward "\\E" nil t)
              (put-text-property (match-beginning 0) ;FIXME
                                 (match-end 0)
                                 'font-lock-face
                                 'font-lock-builtin-face)
              (let* ((end (match-beginning 0))
                     (str (buffer-substring-no-properties begin (1- end)))
                     (char (char-to-string (char-before end))))
                (rxt-seq (list (rxt-string str)
                               (rxt-parse-quantifiers (rxt-string char))))))))
     ;; Character classes: word, digit, whitespace
     ((rx "\\w") (cons 'font-lock-constant-face rxt-pcre-word-chars))
     ((rx "\\W") (cons 'font-lock-constant-face rxt-pcre-non-word-chars))
     ((rx "\\d") (cons 'font-lock-constant-face rxt-digit-chars))
     ((rx "\\D") (cons 'font-lock-constant-face rxt-non-digit-chars))
     ((rx "\\h") (cons 'font-lock-constant-face rxt-pcre-horizontal-whitespace-chars))
     ((rx "\\H") (cons 'font-lock-constant-face rxt-pcre-non-horizontal-whitespace-chars))
     ((rx "\\s") (cons 'font-lock-constant-face rxt-space-chars))
     ((rx "\\S") (cons 'font-lock-constant-face rxt-non-space-chars))
     ((rx "\\v") (cons 'font-lock-constant-face rxt-pcre-vertical-whitespace-chars))
     ((rx "\\V") (cons 'font-lock-constant-face rxt-pcre-non-vertical-whitespace-chars))
     ;; \ + digits: backreference or octal char?
     ((rx "\\" (submatch (+ (any "0-9"))))
      (let* ((digits (match-string 1))
             (dec (string-to-number digits)))
        ;; from "man pcrepattern": If the number is less than 10, or if
        ;; there have been at least that many previous capturing left
        ;; parentheses in the expression, the entire sequence is taken
        ;; as a back reference.
        (if (and (> dec 0)
                 (or (< dec 10)
                     (>= rxt-subgroup-count dec)))
            (cons 'font-lock-variable-name-face (rxt-backref dec))
          ;; from "man pcrepattern": if the decimal number is greater
          ;; than 9 and there have not been that many capturing
          ;; subpatterns, PCRE re-reads up to three octal digits
          ;; following the backslash, and uses them to generate a data
          ;; character. Any subsequent digits stand for themselves.
          (goto-char (match-beginning 1))
          (re-search-forward (rx (** 0 3 (any "0-7"))))
          (cons nil                     ; FIXME
                (rxt-string (char-to-string (string-to-number (match-string 0) 8)))))))
     ;; Other escaped characters
     ((rx "\\" (submatch nonl)) (cons nil (rxt-string (match-string 1))))
     ;; Everything else
     ((rx (or (any "\n") nonl)) (cons nil (rxt-string (match-string 0))))))))

(defun rxt-parse-escapes/pcre ()
  "Consume a one-char PCRE escape at point and return its codepoint equivalent.

Handles only those character escapes which have the same meaning
in character classes as outside them."
  (rxt-fontify-token-case
   ((rx "\\a") (cons 'font-lock-constant-face #x07)) ; bell
   ((rx "\\e") (cons 'font-lock-constant-face #x1b)) ; escape
   ((rx "\\f") (cons 'font-lock-constant-face #x0c)) ; formfeed
   ((rx "\\n") (cons 'font-lock-constant-face #x0a)) ; linefeed
   ((rx "\\r") (cons 'font-lock-constant-face #x0d)) ; carriage return
   ((rx "\\t") (cons 'font-lock-constant-face #x09)) ; tab
   ;; Control character
   ((rx "\\c" (submatch nonl))
    ;; from `man pcrepattern':
    ;; The precise effect of \cx is as follows: if x is a lower case
    ;; letter, it is converted to upper case.  Then bit 6 of the
    ;; character (hex 40) is inverted.
    (cons 'font-lock-constant-face
          (logxor (string-to-char (upcase (match-string 1))) #x40)))
   ;; Hex escapes
   ((rx "\\x" (submatch (** 1 2 (any "0-9" "A-Z" "a-z"))))
    (cons 'font-lock-constant-face (string-to-number (match-string 1) 16)))
   ((rx "\\x{" (submatch (* (any "0-9" "A-Z" "a-z"))) "}")
    (cons 'font-lock-constant-face (string-to-number (match-string 1) 16)))))

(defun rxt-extended-flag-p (flags)
  (if (string-match-p "x" flags) t nil))

(defun rxt-s-flag-p (flags)
  (if (string-match-p "s" flags) t nil))

(defun rxt-parse-subgroup/pcre ()
  (cl-block nil
    (let ((shy nil)
          (x rxt-pcre-extended-mode)
          (s rxt-pcre-s-mode)
          (subgroup-begin (1- (point))))
      (rxt-extended-skip)
      ;; Check for special (? ..) and (* ...) syntax
      (rxt-token-case
       ;; Special (? ... ) groups
       ((rx "?")                        ; (? ... )
        (rxt-token-case
         ;; Shy group (?: ...)
         (":" (setq shy t))
         ;; Comment (?# ...)
         ("#"
          (search-forward ")")
          (put-text-property subgroup-begin (point)
                             'font-lock-face
                             'font-lock-comment-face)
          (cl-return rxt-empty-string))
         ;; Set modifiers (?xs-sx ... )
         ((rx (or
               (seq (group (* (any "xs"))) "-" (group (+ (any "xs"))))
               (seq (group (+ (any "xs"))))))
          (let ((begin (match-beginning 0))
                (on (or (match-string 1) (match-string 3)))
                (off (or (match-string 2) "")))
            (if (rxt-extended-flag-p on) (setq x t))
            (if (rxt-s-flag-p on) (setq s t))
            (if (rxt-extended-flag-p off) (setq x nil))
            (if (rxt-s-flag-p off) (setq s nil))
            (rxt-token-case
             (":" (setq shy t))   ; Parse a shy group with these flags
             (")"
              ;; Set modifiers here to take effect for the remainder
              ;; of this expression; they are let-bound in
              ;; rxt-parse-exp
              (setq rxt-pcre-extended-mode x
                    rxt-pcre-s-mode s)
              (cl-return rxt-empty-string))
             (t
              (rxt-error "Unrecognized PCRE extended construction (?%s...)"
                         (buffer-substring-no-properties begin (point)))))))
         (t (rxt-error "Unrecognized PCRE extended construction ?%c"
                       (char-after)))))
       ;; No special "(* ...)" verbs are recognised
       ((rx "*")
        (let ((begin (point)))
          (search-forward ")")
          (rxt-error "Unrecognized PCRE extended construction (*%s"
                     (buffer-substring begin (point))))))
      ;; Parse the remainder of the subgroup
      (unless shy (cl-incf rxt-subgroup-count))
      (let* ((rxt-pcre-extended-mode x)
             (rxt-pcre-s-mode s)
             (rx (rxt-parse-exp)))
        (rxt-extended-skip)
        (rxt-fontify-token-case
         (")" (cons 'font-lock-builtin-face
                    (if shy rx (rxt-submatch rx))))
         (t (rxt-error "Subexpression missing close paren")))))))

(defun rxt-parse-subgroup/el ()
  (let ((kind
         (rxt-fontify-token-case
          ((rx "?:")
           (cl-incf rxt-subgroup-count)
           (cons 'font-lock-builtin-face 'shy))
          ((rx "?" (group (+ (in "0-9"))) ":")
           (cons 'font-lock-builtin-face
                 (let ((n (string-to-number (match-string 1))))
                   (when (< rxt-subgroup-count n)
                     (setf rxt-subgroup-count n))
                   n)))
          ((rx "?") ; Reserved
           (rxt-error "Unknown match group sequence")))))
    (let ((rx (rxt-parse-exp)))
      (rxt-fontify-token-case
       ((rx "\\)")
        (cons 'font-lock-builtin-face
              (cond ((eq kind 'shy) rx)
                    ((numberp kind)
                     (rxt-submatch-numbered kind rx))
                    (t (rxt-submatch rx)))))
       (t (rxt-error "Subexpression missing close paren"))))))

(defun rxt-parse-braces ()
  (rxt-fontify-token-case
   (rxt-m-to-n-brace-regexp
    (cons 'font-lock-keyword-face
          (list (string-to-number (match-string 1))
                (string-to-number (match-string 2)))))
   (rxt-m-to-?-brace-regexp
    (cons 'font-lock-keyword-face
          (list (string-to-number (match-string 1)) nil)))
   (rxt-m-brace-regexp
    (cons 'font-lock-keyword-face
          (let ((a (string-to-number (match-string 1))))
            (list a a))))
   (t
    (let ((begin (point)))
      (search-forward "}" nil 'go-to-end)
      (rxt-error "Bad brace expression {%s"
                 (buffer-substring-no-properties begin (point)))))))

;; Parse a character set range [...]
(defun rxt-parse-char-class ()
  (when (eobp)
    (rxt-error "Missing close right bracket in regexp"))

  (rxt-syntax-tree-value
   (let* ((negated (rxt-fontify-token-case
                    ("\\^" (cons 'font-lock-warning-face t))
                    (t (cons nil nil))))
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
           (rxt-error "Missing close right bracket in regexp"))

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
     (put-text-property (point) (1+ (point))
                        'font-lock-face 'font-lock-builtin-face)
     (forward-char)
     result)))

;; Within a charset, parse a single character, a character range or a
;; posix class. Returns the character (i.e. an integer), a cons (from
;; . to), or a symbol denoting the posix class
(defun rxt-parse-char-class-piece ()
  (let ((atom (rxt-parse-char-class-atom)))
    (if (and (integerp atom)
             (looking-at (rx (submatch "-") (not (any "]")))))
        (let ((r-end (save-match-data (rxt-maybe-parse-range-end))))
          (if (not r-end)
              atom
            (put-text-property (match-beginning 1) (match-end 1)
                               'font-lock-face
                               'font-lock-builtin-face)
            (cons atom r-end)))
      atom)))

;; Parse a single character or named class within a charset
;;
;; Doesn't treat ] or - specially -- that's taken care of in other
;; functions.
(defun rxt-parse-char-class-atom ()
  (or
   ;; First, check for PCRE-specific backslash sequences
   (and rxt-parse-pcre
        (rxt-parse-char-class-atom/pcre))
   ;; Char-class syntax
   (rxt-fontify-token-case
    ;; Named classes [:alnum:], ...
    (rxt-named-classes-regexp
     (cons 'font-lock-constant-face
           (intern (match-string 1))))
    ;; Error on unknown posix-class-like syntax
    ((rx "[:" (* (any "a-z")) ":]")
     (rxt-error "Unknown posix class %s" (match-string 0)))
    ;; Error on [= ... ]= collation syntax
    ((rx "[" (submatch (any "." "="))
         (* (any "a-z")) (backref 1) "]")
     (rxt-error "%s collation syntax not supported" (match-string 0)))
    ;; Other characters stand for themselves
    ((rx (or "\n" nonl))
     (cons font-lock-string-face (string-to-char (match-string 0)))))))

;; Parse backslash escapes inside PCRE character classes
(defun rxt-parse-char-class-atom/pcre ()
  (or (rxt-parse-escapes/pcre)
      (rxt-token-case
       ;; Backslash + digits => octal char
       ((rx "\\" (submatch (** 1 3 (any "0-7"))))
        (string-to-number (match-string 1) 8))
       ;; Various character classes.
       ((rx "\\d") rxt-digit-chars)
       ((rx "\\D") rxt-non-digit-chars)
       ((rx "\\h") rxt-pcre-horizontal-whitespace-chars)
       ((rx "\\H") rxt-pcre-non-horizontal-whitespace-chars)
       ((rx "\\s") rxt-pcre-whitespace-chars)
       ((rx "\\S") rxt-pcre-non-whitespace-chars)
       ((rx "\\v") rxt-pcre-vertical-whitespace-chars)
       ((rx "\\V") rxt-pcre-non-vertical-whitespace-chars)
       ((rx "\\w") rxt-pcre-word-chars)
       ((rx "\\W") rxt-pcre-non-word-chars)
       ;; "\b" inside character classes is a backspace
       ((rx "\\b") ?\C-h)
       ;; Ignore other escapes
       ((rx "\\" (submatch nonl))
        (string-to-char (match-string 1))))))


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



;;;; 'Unparsers' to convert the syntax tree back to concrete `rx' or SRE syntax

;;; ADT -> rx notation
(defconst rxt-rx-verbose-equivalents
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

          ((rxt-submatch-numbered-p re)
           (if (rxt-seq-p (rxt-submatch-numbered-p re))
               (cl-list* (rxt-rx-symbol 'submatch-n)
                         (rxt-submatch-numbered-n re)
                         (mapcar #'rxt-adt->rx
                                 (rxt-seq-elts
                                  (rxt-submatch-numbered-body re))))
             (list (rxt-rx-symbol 'submatch-n)
                   (rxt-submatch-numbered-n re)
                   (rxt-adt->rx (rxt-submatch-numbered-body re)))))

          ((rxt-backref-p re)
           (let ((n (rxt-backref-n re)))
             (if (<= n 9)
                 (list 'backref (rxt-backref-n re))
               (rxt-error "Too many backreferences (%s)" n))))

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
                         `(maximal-match ,rx))
                     `(minimal-match ,rx)))
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
           (rxt-error "No RX translation for %s" re)))))

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
        (rxt-error "No SRE translation of non-greedy repetition %s" re))
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
    (rxt-error "No SRE translation for %s" re))))


;;;; 'Unparser' to PCRE notation

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
  (cl-destructuring-bind (s lev) (rxt-adt->pcre/lev re) s))

(defun rxt-adt->pcre/lev (re)
  (cond
   ((rxt-primitive-p re)
    (let ((s (rxt-primitive-pcre re)))
      (if s
          (list s 1)
        (rxt-error "No PCRE translation for %s" re))))

   ((rxt-string-p re) (rxt-string->pcre re))
   ((rxt-seq-p re) (rxt-seq->pcre re))
   ((rxt-choice-p re) (rxt-choice->pcre re))

   ((rxt-submatch-p re) (rxt-submatch->pcre re))
   ((rxt-backref-p re)
    (list (format "\\%d" (rxt-backref-n re)) 1))

   ((rxt-repeat-p re) (rxt-repeat->pcre re))

   ((or (rxt-char-set-union-p re)
        (rxt-char-set-negation-p re))
    (rxt-char-set->pcre re))

   ;; FIXME
   ;; ((rxt-char-set-intersection re) (rxt-char-set-intersection->pcre re))

   (t
    (rxt-error "No PCRE translation for %s" re))))

(defconst rxt-pcre-metachars (rx (any "\\^.$|()[]*+?{}")))
(defconst rxt-pcre-charset-metachars (rx (any "]" "[" "\\" "^" "-")))

(defun rxt-string->pcre (re)
  (let ((chars (rxt-string-chars re)))
    (list
     (replace-regexp-in-string
      rxt-pcre-metachars
      "\\\\\\&" chars)
     ;; A one-character string is a 'piece' (it binds to a following
     ;; quantifier).  A longer string is a 'branch' (it has to be
     ;; enclosed in parentheses to bind to a following quantifier).
     (if (> (length chars) 1) 2 1))))

(defun rxt-seq->pcre (re)
  (let ((elts (rxt-seq-elts re)))
    (if (null elts)
        ""
      (rxt-seq-elts->pcre elts))))

(defun rxt-seq-elts->pcre (elts)
  (cl-destructuring-bind
      (s lev) (rxt-adt->pcre/lev (car elts))
    (if (null (cdr elts))
        (list s lev)
      (cl-destructuring-bind
          (s1 lev1) (rxt-seq-elts->pcre (cdr elts))
        (list (concat (rxt-paren-if-necessary s lev)
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
  (cl-destructuring-bind
      (s lev) (rxt-adt->pcre/lev (car elts))
    (if (null (cdr elts))
        (list s lev)
      (cl-destructuring-bind
          (s1 lev1) (rxt-choice-elts->pcre (cdr elts))
        (list (concat s "|" s1) 3)))))

(defun rxt-submatch->pcre (re)
  (cl-destructuring-bind
      (s lev) (rxt-adt->pcre/lev (rxt-submatch-body re))
    (list (concat "(" s ")") 0)))

(defun rxt-repeat->pcre (re)
  (let ((from (rxt-repeat-from re))
        (to (rxt-repeat-to re))
        (body (rxt-repeat-body re))
        (greedy (rxt-repeat-greedy re)))
    (cl-destructuring-bind
        (s lev) (rxt-adt->pcre/lev body)
      (cond
       ((and to (= from 1) (= to 1)) (list s lev))
       ((and to (= from 0) (= to 0)) (list "" 2))
       (t
        (when (> lev 1)     ; parenthesize non-atoms
          (setq s (concat "(?:" s ")")
                lev 0))
        (list (if to
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
         (list
          (concat "[" (rxt-char-set->pcre/chars re) "]") 1))

        ((rxt-char-set-negation-p re)
         (let ((elt (rxt-char-set-negation-elt re)))
           (if (rxt-char-set-union-p elt)
               (list
                (concat "[^" (rxt-char-set->pcre/chars elt) "]") 1)
             (rxt-error "No PCRE translation of %s" elt))))

        (t
         (rxt-error "Non-char-set in rxt-char-set->pcre: %s" re))))

;; Fortunately, easier in PCRE than in POSIX!
(defun rxt-char-set->pcre/chars (re)
  (cl-flet
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
   ((rxt-submatch-numbered-p re)
    (rxt-adt->strings (rxt-submatch-numbered-body re)))

   ((rxt-repeat-p re) (rxt-repeat->strings re))

   ((rxt-char-set-union-p re) (rxt-char-set->strings re))

   (t
    (error "Can't generate productions of %s"
           (rxt-syntax-tree-readable re)))))

(defun rxt-concat-product (heads tails)
  (cl-mapcan
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
        (error "Can't generate all productions of unbounded repeat \"%s\""
               (rxt-syntax-tree-readable re))
      (let ((strings (rxt-adt->strings (rxt-repeat-body re))))
        (rxt-repeat-n-m->strings from to strings)))))

(defun rxt-repeat-n-m->strings (from to strings)
  (cond
   ((zerop to) '(""))
   ((= to from) (rxt-repeat-n->strings from strings))
   (t           ; to > from
    (let* ((strs-n (rxt-repeat-n->strings from strings))
           (accum (cl-copy-list strs-n)))
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
      (error "Can't generate all productions of named character classes in \"%s\""
             (rxt-syntax-tree-readable re))
    (let ((chars (mapcar #'char-to-string (rxt-char-set-union-chars re))))
      (dolist (range (rxt-char-set-union-ranges re))
        (let ((end (cdr range)))
          (cl-do ((i (car range) (+ i 1)))
              ((> i end))
            (push (char-to-string i) chars))))
      chars)))


;;;; Regexp fontification in Elisp buffers

(defun rxt-fontify-regexp-at-point (remove-all-p)
  "Toggle parser-based font-locking of the Elisp regexp nearest point.

With prefix argument, removes font-locking for all regexps in the
current buffer.

As long as regexp font-locking is turned on for a particular
string, any changes to its contents will be reflected in the

You can enable fontifying for as many regexps as you like, but
because the font-locking is implemented using overlays, buffer
updates may become slow if too many are active at once."
  (interactive "P")
  (let ((existing-overlay (rxt--get-overlay-at-point)))
    (cond (remove-all-p (rxt--remove-overlays))
          (existing-overlay (rxt--delete-overlay existing-overlay))
          (t
           (save-excursion
             ;; Put point before the opening " of the string nearest point
             (let ((string-start (nth 8 (syntax-ppss))))
               (cond (string-start (goto-char string-start))
                     ((looking-back "\"") (backward-sexp))
                     ((looking-at "\"") nil))
               (rxt--fontify-regexp-at-point1))))))
  (set (make-local-variable 'font-lock-syntactic-face-function)
       'rxt--syntactic-face-function))

(defun rxt--get-overlay-at-point ()
  (cdr (get-char-property-and-overlay (point) 'rxt-fontified-regexp)))

(defun rxt--delete-overlay (overlay)
  (save-excursion
    (let ((start (overlay-start overlay))
          (end   (overlay-end overlay)))
      (delete-overlay overlay)
      (font-lock-fontify-region start end))))

(defun rxt--fontify-regexp-at-point1 ()
  ;; Point should be before the opening " of an Elisp string. Reads
  ;; the regexp from the buffer, parses it and copies the
  ;; fontification back to the buffer.
  (let ((regex (save-excursion (read (current-buffer)))))
    (condition-case err
        (cl-destructuring-bind (ast fontified)
            (rxt-parse-and-fontify regex)
          (rxt--copy-fonts-to-read-syntax fontified))
      (error (message "Invalid regexp: %s" (cadr err))))))

(defun rxt--copy-fonts-to-read-syntax (fontified)
  (cl-destructuring-bind (begin . end)
      (bounds-of-thing-at-point 'sexp)
    (unless (rxt--get-overlay-at-point)
      (let ((overlay
             (make-overlay begin end (current-buffer)
                           t nil)))
        (overlay-put overlay 'rxt-fontified-regexp t)
        (overlay-put overlay 'face 'rxt-highlight-face)
        (overlay-put overlay 'modification-hooks
                     '(rxt--refontify-regexp))))
    (skip-syntax-forward "\"")
    (cl-loop for chunk in (split-string fontified "" t)
             do
             (progn
               (let ((start (point))
                     (end
                      (progn
                        ;; Move point over the next char or escape sequence
                        (rxt-token-case
                         ((rx "\\u" (= 4 (any xdigit))))
                         ((rx "\\U00" (= 6 (any xdigit))))
                         ((rx "\\" (** 1 3 (any "0-7"))))
                         ((rx (one-or-more "\\"
                                           (any "C" "M" "S" "H" "A")
                                           "-")
                              any))
                         ((rx "\\" any))
                         ((rx any)))
                        (point)))
                     (face (get-text-property 0 'font-lock-face chunk)))
                 (put-text-property start end 'font-lock-face face))))
    (font-lock-fontify-block 1)))

(defun rxt--remove-overlays ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (cl-loop do (goto-char
                   (next-single-char-property-change
                    (point) 'rxt-fontified-regexp))
               until (eobp)

               do (cl-destructuring-bind (prop . overlay)
                      (get-char-property-and-overlay
                       (point) 'rxt-fontified-regexp)
                    (when overlay
                      (goto-char (overlay-end overlay))
                      (rxt--delete-overlay overlay)))))
    (font-lock-fontify-buffer)))

(defvar rxt--timer nil)

(defun rxt--refontify-regexp (overlay after-p start end &optional len)
  (when rxt--timer
    (cancel-timer rxt--timer))
  (setq rxt--timer
        (run-with-timer 0.1 nil
                        (apply-partially 'rxt--refontify-regexp1
                                         overlay))))

(defun rxt--refontify-regexp1 (overlay)
  (let ((start (overlay-start overlay))
        (end   (overlay-end overlay)))
    (let ((inhibit-modification-hooks t))
      (remove-text-properties start end 'font-lock-face)
      (save-excursion
        (goto-char start)
        (rxt--fontify-regexp-at-point1)))))

(defun rxt--syntactic-face-function (context)
  (if (get-char-property (point) 'rxt-fontified-regexp)
      nil
    (lisp-font-lock-syntactic-face-function context)))



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
                  (cl-case syntax
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
