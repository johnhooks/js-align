# -*- Makefile -*-

EMACS = emacs

# Compile with noninteractive and relatively clean environment.
BATCHFLAGS = -batch -Q

SRCS = js-align.el

OBJS = $(SRCS:.el=.elc)

%.elc: %.el
	${EMACS} $(BATCHFLAGS) -L . -f batch-byte-compile $^

all: $(OBJS)

clean:
	-rm -f $(OBJS)

test:
	${EMACS} $(BATCHFLAGS) -L . -l js-align.el -l tests/indent.el\
	  -l tests/regexps.el -f ert-run-tests-batch-and-exit
