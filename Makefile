default:
	cabal build -j

dependencies:
	cabal install --only-dep --enable-test -j

configure:
	cabal configure --enable-test

watch:
	find . | grep .hs$ | entr cabal test --show-details=always --test-option=--color

test:
	cabal test --show-details=always --test-option=--color
