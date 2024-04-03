# Changelog for graphex

## Unreleased changes

- `graphex graph all` now fails when no modules match the query.

## 0.1.2.1

- Fix `graphex cabal` for units with multiple source directories.
  - It would mistakenly treat some modules as having no file.
- Looking Glass conversion had a bug where nodes that only appeared as children wouldn't get `nodes`.
  - This caused querying a `graphex cabal --include-external` graph to fail due to `Data.Map.!` crashes
    during conversion from Looking Glass.

## 0.1.2.0

- The graph edge direction has been standardized. `A -> B` means "A imports B."
  - `graphex graph all X` gives you all transitively-imported modules of X.
  - `graphex graph -r all X` gives you all modules that transitively import X.
- `graphex graph why` output is a bit clearer: It explicitly says what imports what.
- `graphex graph why` does not care about node order now - it tries both directions.
- `graphex graph why --all` outputs a tree of all paths between the specified nodes.
- `-g` now takes `-` to mean stdin.
- `graphex cabal` can discover executable and test suite modules.
- `graphex cabal` is multithreaded (configurable with `--jobs`/`-j`).
- `graphex cabal` now has more configuration options to control which modules are discovered.
- `graphex cabal` now has `--paths`/`-p` which uses file paths as nodes instead of module names.
- `graphex graph all -r` uses regex to pick which modules to analyze.
- New command: `graphex graph all-paths`. Output new graph JSON restricted to just the paths between the specified nodes.
- New command: `graphex graph longest`. Find the longest path between nodes.
- New command: `graphex graph cat`. Combine graphs (using a union).
- New command: `graphex graph remove`. Remove nodes from a graph.
- The import parser now uses `Text` instead of `String` for a free ~25% speedup.
- Looking Glass node attributes are now preserved by operations that transform graphs such as `remove`, `cat`, and `select`.
- `graphex` will log to stderr if the `GRAPHEX_VERBOSITY` environment variable is set.
- New `Graphex.Search` module (instead of using `search-algorithms`).
- Lots of bugfixing & additional testing.


## 0.1.0.0

Initial release
