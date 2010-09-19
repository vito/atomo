all: install

install:
	mkdir -p ${HOME}/.eco/{bin,lib}
	cp bin/eco ${HOME}/.eco/bin/eco
	chmod +X ${HOME}/.eco/bin/eco
	cabal install

