default:
	elm-make src/*.elm --output static/main.js
	cabal run

test:
	cabal test
