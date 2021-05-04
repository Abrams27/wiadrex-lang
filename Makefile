CABAL = cabal


all:
	${CABAL} install --install-method=copy --installdir=./


clean:
	${CABAL} clean
	rm interpreter
