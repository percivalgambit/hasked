all: .setup-complete

.PHONY: hasked run build test clean

hasked: run

run: setup
	cabal run

build: setup
	cabal build

test: setup
	cabal test

clean:
	cabal clean

setup: .setup-complete

.setup-complete:
	cabal update
	cabal sandbox init
	cabal install --only-dependencies --enable-tests
	touch $@
