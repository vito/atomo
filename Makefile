all: install

install: install-eco
	cabal install

install-eco:
	mkdir -p ${HOME}/.eco/{bin,lib}
	cp bin/eco ${HOME}/.eco/bin/eco
	chmod +X ${HOME}/.eco/bin/eco
