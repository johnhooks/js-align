# -*- Makefile -*-

EMACS = emacs

# Add the newer Org mode to the load path.
TANGLEFLAGS = --batch -l configure.el $^

# Compile with noninteractive and relatively clean environment.
BATCHFLAGS = -batch -Q

ORGS = js-align.org

SRCS = $(ORGS:.org=.el)

OBJS = $(SRCS:.el=.elc)

%.el: %.org
	${EMACS} ${TANGLEFLAGS} -f org-babel-tangle

#--eval '(org-babel-tangle "$^")'

%.elc: %.el
	${EMACS} $(BATCHFLAGS) -L . -f batch-byte-compile $^

all: $(OBJS)

clean:
	-rm -f $(OBJS)

test:
	${EMACS} $(BATCHFLAGS) -L . -l js-align.el -l tests/indent.el\
	  -l tests/regexps.el -f ert-run-tests-batch-and-exit
