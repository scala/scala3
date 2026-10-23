# Refinement types for Scala

Qualified types are an experimental feature allowing to refine types with logical predicates.

```scala
val x: {res: Int with res > 0} = 42  // explicit binder
val x2: Int with x2 > 0 = 42  // implicit binder
```

We call them "qualified types" rather than "refinement types" to avoid confusion with Scala/DOT's structural refinements.

## Running

```
sbtn "scalac -language:experimental.qualifiedTypes tests/pos-custom-args/qualified-types/list_collect.scala"
```

For strict checks, add: `-Ycheck:all -Ytest-pickler -YcheckQualifiedTypes`

## Tests

### Compilation tests

Test files: `tests/{pos,neg,warn,run}-custom-args/qualified-types`. The `qualified-types` filter matches any subpath; use e.g. `list_collect` to run a single test.

```
sbtn "scala3-nonbootstrapped/testCompilation qualified-types"
sbtn "scala3-bootstrapped/testCompilation qualified-types"
```

- Also run with coverage: `--enable-coverage-phase`
- To update checkfiles: `--update-checkfiles`

### Printing tests

Test files: `tests/printing/qualified-types*.scala`.

```
sbtn "scala3-compiler-nonbootstrapped/testOnly *Printing*"
```

To update checkfiles: append `-- -Ddotty.tests.updateCheckfiles=TRUE`.
Note checkfiles must be created manually for new tests.

### Unit tests

```
sbtn "scala3-compiler-nonbootstrapped/testOnly *ENode*"
sbtn "scala3-compiler-nonbootstrapped/testOnly *EGraph*"
```

## Formatting

```
scala-cli format compiler/src/dotty/tools/dotc/qualified_types
```

## Branches

- `qualifiers-ci` ([mbovel/dotty](https://github.com/mbovel/dotty/tree/qualifiers-ci)) is the development branch, opened as [scala/scala3#21586](https://github.com/scala/scala3/pull/21586). Its last commit must always be "[Temporary] Enable qualified types by default".
- `refinement-types` ([epfl-lara/dotty](https://github.com/epfl-lara/dotty/tree/refinement-types)) is the release branch for the Stainless fork. It is the same as `qualifiers-ci`, but with "[fork-only] CI Setup" instead of "[Temporary] Enable qualified types by default".

`refinement-types` is regularly updated from `qualifiers-ci`:

```
git rebase --onto qualifiers-ci~1 refinement-types~1 refinement-types
```

## Development guidelines

- Commands in "Tests" and "Formatting" above must successfully pass before every commit.
- Markdown must be used in commit titles and messages.
- Doc comments must be concise. They must document high-level behavior and design decisions, not implementation details.
- Add LLM as co-author only if it made significant design decisions (e.g.
  "investigate #12345 and fix it"), not for mechanical work (e.g. "use a type
  accumulator in `containsQualifier`"). When in doubt, include it.
- Keep the implementation self-contained in the `qualified_types` package. Outside of it, add only minimal _hooks_ that delegate to the package, and avoid comments.
- Don't use wildcard imports; enumerate the imported members explicitly.
