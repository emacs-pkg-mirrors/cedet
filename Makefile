## Makefile --- Definition file for building CEDET
##
## Copyright (C) 2003 by David Ponce
##
## Author: David Ponce <david@dponce.com>
## Maintainer: CEDET developers <http://sf.net/projects/cedet>
## Created: 12 Sep 2003
## X-RCS: $Id: Makefile,v 1.3 2003/09/23 15:13:57 ponced Exp $
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 2, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with GNU Emacs; see the file COPYING.  If not, write to the
## Free Software Foundation, Inc., 59 Temple Place - Suite 330,
## Boston, MA 02111-1307, USA.

######## You can customize this part of the Makefile ########

## The directory where CEDET is installed
CEDET_HOME=$(CURDIR)

## The CEDET's packages installed
CEDET_PACKAGES=\
common \
ede \
speedbar \
eieio \
semantic \
cogre

## Path to your Emacs
EMACS=emacs

## Your shell (On Windows/Cygwin I recommend to use bash)
#SHELL=bash

## Path to your find and rm commands
FIND=find
#RM = rm -f

############### Internal part of the Makefile ###############
__BUILD_AUTOLOADS=$(patsubst %,%-autoloads,$(CEDET_PACKAGES))
__CLEAN_AUTOLOADS=$(patsubst %,clean-%,$(__BUILD_AUTOLOADS))
__DOMAKE=$(MAKE) $(MFLAGS) EMACS="$(EMACS)" SHELL="$(SHELL)"

## Build
##

all: $(CEDET_PACKAGES)

.PHONY: $(CEDET_PACKAGES)
$(CEDET_PACKAGES):
	cd $(CEDET_HOME)/$@ && $(__DOMAKE)

## Update
##

autoloads: $(__BUILD_AUTOLOADS)

.PHONY: $(__BUILD_AUTOLOADS)
$(__BUILD_AUTOLOADS):
	cd $(CEDET_HOME)/$(firstword $(subst -, ,$@)) && \
	$(__DOMAKE) autoloads

recompile: autoloads
	cd $(CEDET_HOME) && \
	$(EMACS) -batch -q --no-site-file -l common/cedet.el \
	-f batch-byte-recompile-directory $(CEDET_PACKAGES)

## Cleanup
##

clean-autoloads: $(__CLEAN_AUTOLOADS)

.PHONY: $(__CLEAN_AUTOLOADS)
$(__CLEAN_AUTOLOADS):
	$(FIND) $(CEDET_HOME)/$(word 2,$(subst -, ,$@)) -type f \
	-name "*-loaddefs.el" \
	-print -exec $(RM) {} \;

.PHONY: clean-grammars
clean-grammars:
	$(FIND) $(CEDET_HOME) -type f -name "*-[bw]y.el" \
	! -name "semantic-grammar-wy.el" \
	-print -exec $(RM) {} \;

.PHONY: clean-info
clean-info:
	$(FIND) $(CEDET_HOME) -type f -name "*.info*" \
	-print -exec $(RM) {} \;

.PHONY: clean-elc
clean-elc:
	$(FIND) $(CEDET_HOME) -type f -name "*.elc" \
	-print -exec $(RM) {} \;

.PHONY: clean
clean:
	$(FIND) $(CEDET_HOME) -type f \( -name "*-script" -o -name "*~" \) \
	-print -exec $(RM) {} \;

clean-all: clean clean-elc clean-info clean-grammars clean-autoloads

# Makefile ends here
