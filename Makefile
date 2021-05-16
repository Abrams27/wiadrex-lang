CABAL = cabal


all:
	${CABAL} build -j1
	mv dist/build/interpreter/interpreter interpreter



install:
	${CABAL} install --install-method=copy --installdir=./ --overwrite-policy=always


clean:
	${CABAL} clean
	rm interpreter
