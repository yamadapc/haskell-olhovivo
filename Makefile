default:
	cabal build -j

dependencies:
	cabal install --only-dep --enable-test -j

coverage:
	cabal install --only-dep --enable-test --enable-coverage -j
	cabal configure --enable-test --enable-coverage
	cabal build
	cabal test --show-details=always --test-option=--color

configure:
	cabal configure --enable-test

watch:
	find . | grep .hs$ | entr cabal test --show-details=always --test-option=--color

test:
	cabal test --show-details=always --test-option=--color
