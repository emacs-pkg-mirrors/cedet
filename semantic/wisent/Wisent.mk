#
# Wisent Specific inference rules
#

WY_COMP:=${subst .wy,-wy.el, ${wy_MISC}}

${WY_COMP}: wisent-make-script

wisent-make-script: Wisent.mk
	@echo "(add-to-list 'load-path nil)" > $@
	@for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@; \
	done;
	@echo "(require 'semantic-load)" >> $@
	@echo "(require 'semantic-grammar)" >> $@
	@echo "(setq debug-on-error t)" >> $@

%-wy.el: %.wy
	${EMACS} -batch -q -l wisent-make-script $< -f semantic-grammar-create-package -f save-buffer

languages: ${WY_COMP}

#end