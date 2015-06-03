all: dependencies

caskdir = ${PWD}/.cask
cask    = $(caskdir)/cask/bin/cask

$(caskdir):
	mkdir $@
$(cask): $(caskdir)
	cd .cask && git clone https://github.com/cask/cask.git

.PHONY: dependencies
dependencies: $(cask)
	$(cask) install
