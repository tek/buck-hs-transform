# About

This tool reduces a Haskell Buck project to its dependency graph skeleton for profiling purposes.
The resulting project is a shallow list of directories, each corresponding to a Buck target in the original project,
eliminating nested packages.
Both targets and modules are given generic names lib `Lib4Mod7`, stripping each module down to its imports.
Imports of modules from external dependencies are replaced with imports from a set of dummy packages named `dep<N>`,
with `N` ranging from 1 through 100.

# Implementation

The directory `tests/tree1` contains an example test case consisting of a Buck project in the subdirectory `original`.
These instructions write an intermediate JSON metadata tree in `json` and the final product in `out`.

1. Collect Haskell metadata in the original project:
   ```bash
   nix develop '.#parse' -c python parser.py --in 'tests/tree1/original' --out 'tests/tree1/json' local-packages src
   ```
   This replicates the original module tree, replacing each module with a JSON file containing only its module name
   and imports.
   The positional arguments allow restricting the subdirectories scanned for Haskell files.

1. Collect Buck metadata:
   ```bash
   buck uquery -B --output-format json "kind('haskell_library|export_file', deps('$target'))" > buck.json
   ```
   Substitute `$target` with the Buck
   [target expression](https://buck2.build/docs/users/query/uquery/#target-expression) enumerating the desired targets.
   The simplest case would be `//...`, while the example in `tests/tree1` uses `//tests/tree1/original/...`.
   Move the resulting `buck.json` to the root of the JSON tree created in the first step, e.g.
   `tests/tree1/json/buck.json`.

1. Run the transformer:
   ```bash
   nix run . -- --original 'tests/tree1/original' --json 'tests/tree1/json' --out 'tests/tree1/out'
   ```
   This program performs the following steps:
   * Parse the JSON files
   * Rename all targets to `lib<L>`
   * Resolve `export_file` labels
   * Rename all modules and matching imports
   * Replace the remaining imports by names like `Dep<N>`
   * Write the new structure to the output directory
   The example output tree will consist of four directories `lib1`, `lib2`, `lib3`, `lib4`, each containing one `BUCK`
   file with one target and Haskell files named `Lib<L>Mod<M>`.

1. Build the resulting project:
   ```bash
   nix develop '.#buck' -c buck build '//tests/tree1/out/...'
   ```
   The Nix environment provides the dummy dependencies `dep1` ... `dep100` used as external packages, as well as Buck
   and the appropriate GHC.
