repl: hpack
	cabal repl
.PHONY: repl

exe-repl: hpack
	cabal repl exe:graphex
.PHONY: exe-repl

hpack: package.yaml
	hpack

install: hpack
	cabal install exe:graphex --overwrite-policy=always
