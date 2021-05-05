CABAL = cabal


all:
	${CABAL} install --install-method=copy --installdir=./ --overwrite-policy=always


clean:
	${CABAL} clean
	rm interpreter
