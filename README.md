# graphex

Simple tool to explore some dependency data.

```
Usage: graphex graph [-g|--graph ARG] [-r|--reverse] COMMAND

  Graph operations

Available options:
  -g,--graph ARG           path to graph data (default: "graph.json")
  -r,--reverse             reverse edges
  -h,--help                Show this help text

Available commands:
  deps                     Show all direct inbound dependencies to a module
  all                      Show all dependencies to a module
  why                      Show why a module depends on another module
  rank                     Show the most depended on modules
  select                   Select a subset of the graph from a starting module
```

Run this from the root of a cabal project to generate the dependency graph JSON:

```
graphex cabal > my-deps.json
```
