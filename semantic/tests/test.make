# Test makefile -*- makefile -*-

top=
ede_FILES=Project.ede Makefile

example_MISC=semantic-skel.el skeleton.bnf
init_LISP=semantic-load.el
DISTDIR=$(top)semantic-$(VERSION)

all: example semantic Languages tools senator semantic.info

example: 
	@

init: $(init_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	@if test ! -z "${LOADPATH}" ; then\
	   for loadpath in ${LOADPATH}; do \
	      echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	    done;\
	fi
	@echo "(setq debug-on-error t)" >> $@-compile-script
	$(EMACS) -batch -l $@-compile-script -f batch-byte-compile $^

include teset.mk

ifdef SOME_SYMBOL
  VAR1 = foo
else
  VAR1 = bar
endif

ifndef SOME_OTHER_SYMBOL
  VAR1 = baz
endif

ifeq ($(VAR1), foo)
  VAR2 = gleep
else
  ifneq ($(VAR1), foo)
    VAR2 = glop
  endif
endif

# End of Makefile
