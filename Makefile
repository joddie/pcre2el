all: pcre2el.elc

pcre2el-tests.elc: pcre2el.elc

%.elc: %.el
	emacs --batch -L . -f batch-byte-compile $<

test: pcre2el-tests.elc
	emacs --batch -L . \
	  --load=pcre2el-tests \
	  --eval='(setq max-lisp-eval-depth 2000)' \
	  --eval='(ert-run-tests-batch "^rxt-")'

.PHONY: all test
