all: init
	cabal build --jobs

check: all
	cabal check
	cabal test

init:
	test -f cabal.sandbox.config || (\
		cabal sandbox init && \
		cabal install --only-dependencies --enable-tests --jobs)

clean:
	-git clean -dfx
