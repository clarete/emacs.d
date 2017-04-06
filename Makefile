all: dependencies

caskgit   = https://github.com/cask/cask.git
caskdir   = ${PWD}/.cask
caskgdir  = $(caskdir)/cask
cask      = $(caskdir)/cask/bin/cask
caskemacs = $(shell which emacs)

ditafile  = ditaa0_9.zip
ditaaurl  = https://downloads.sourceforge.net/project/ditaa/ditaa/0.9/$(ditafile)
ditaaoutd = ${PWD}/contrib/ditaa
ditaaoutf = $(ditaaoutd)/$(ditafile)
ditaa     = $(ditaaoutd)/ditaa0_9.jar

$(caskdir):
	mkdir $@
$(caskgdir): | $(caskdir)
	cd $(caskdir) && git clone $(caskgit)
$(cask): $(caskgdir)

$(ditaaoutd):
	mkdir -p $@
$(ditaaoutf): | $(ditaaoutd)
	cd $(ditaaoutd) && wget $(ditaaurl)
$(ditaa): $(ditaaoutf)
	cd $(ditaaoutd) && unzip $^

.PHONY: dependencies
dependencies: $(ditaa) $(cask)
	CASK_EMACS=$(caskemacs) $(cask) install
