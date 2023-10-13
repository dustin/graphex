repl: graphex.cabal
	cabal repl
.PHONY: repl

exe-repl: graphex.cabal
	cabal repl exe:graphex
.PHONY: exe-repl

test-repl: graphex.cabal
	cabal repl tests
.PHONY: test-repl

graphex.cabal: package.yaml
	hpack

install: graphex.cabal
	cabal install exe:graphex --overwrite-policy=always
.PHONY: install

test: graphex.cabal
	cabal test --test-show-details=streaming
.PHONY: test

fmt:
	fd .hs src --exec stylish-haskell -i
	fd .hs test --exec stylish-haskell -i
	fd .hs app --exec stylish-haskell -i
.PHONY: fmt

graph.json:
	cabal run -v0 exe:graphex -- cabal ${args} > graph.json
.PHONY: graph.json
