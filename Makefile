default:
	./node_modules/.bin/elm-make src/frontend/*.elm --output static/main.js
	cabal build

test:
	cabal test
