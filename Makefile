all: init
	cabal build --jobs

check: all
	cabal check
	cabal test

init:
	cabal sandbox init
	cabal install --only-dependencies --enable-tests --jobs
	cabal configure --enable-tests

clean:
	-git clean -dfx
