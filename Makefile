default:
	elm-make src/*.elm --output main.js

test:
	cabal test
