repl: hpack
	cabal repl
.PHONY: repl

hpack: package.yaml
	hpack
