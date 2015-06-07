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

# since cabal does not install tool dependencies, we need to explicitly install
# them for ncurses
	cabal install alex
	cabal install happy
	cabal install language-c
	cabal install c2hs

	cabal install --only-dependencies --enable-tests
	touch $@
