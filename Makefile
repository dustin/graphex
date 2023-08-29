repl: graphex.cabal
	cabal repl
.PHONY: repl

exe-repl: graphex.cabal
	cabal repl exe:graphex
.PHONY: exe-repl

graphex.cabal: package.yaml
	hpack

install: graphex.cabal
	cabal install exe:graphex --overwrite-policy=always
.PHONY: install

test: graphex.cabal
	cabal test
.PHONY: test

fmt:
	fd .hs --exec stylish-haskell -i
.PHONY: fmt
