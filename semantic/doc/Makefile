
TEXISRC = app-dev-guide.texi installation.texi internals.texi \
	  lang-support-guide.texi overview.texi user-guide.texi \
	  semantic.texi

%.info : %.texi
	makeinfo --no-split $*.texi
#	makeinfo --split-size 200000 $*.texi

all: semantic.info

semantic.info :: $(TEXISRC)

clean:
	rm -f *.info
