all: .setup-complete

.PHONY: hasked run build test clean unsetup

hasked: run

run: setup
	cabal run

build: setup
	cabal build

test: setup
	cabal test

clean:
	cabal clean

unsetup:
	rm -f .setup-complete
	-cabal sandbox delete

setup: .setup-complete

.setup-complete:
	cabal update
	-cabal sandbox init
	cabal install --only-dependencies --enable-tests
	touch $@
