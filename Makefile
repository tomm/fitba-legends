default:
	elm-make src/*.elm --output static/main.js

test:
	cabal test
