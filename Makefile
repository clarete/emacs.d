all: dependencies

caskgit   = https://github.com/cask/cask.git
caskdir   = ${PWD}/.cask
caskgdir  = $(caskdir)/cask
cask      = $(caskdir)/cask/bin/cask
caskemacs = $(shell which emacs)

$(caskdir):
	mkdir $@
$(caskgdir): | $(caskdir)
	cd $(caskdir) && git clone $(caskgit)
$(cask): $(caskgdir)

.PHONY: dependencies
dependencies: $(cask)
	CASK_EMACS=$(caskemacs) $(cask) install
