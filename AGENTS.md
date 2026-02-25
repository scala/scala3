# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Dotty is the Scala 3 compiler (`dotc`). It compiles Scala source code into JVM bytecode (and optionally Scala.js IR). The compiler is itself written in Scala 3.

## Build Commands

Use `sbtn` (sbt with native thin client) to run commands. This keeps an sbt server running for faster subsequent invocations.

### Compilation
- `sbtn scalac <file>` - Compile a file with the current compiler changes
- `sbtn scala <class>` - Run a compiled class

Use the `local/` directory for temporary test files (gitignored).

### Testing
- `sbtn testCompilation` - Run the whole compilation test suite.
- `sbtn testCompilation <name>` - Run compilation tests matching `<name>` (e.g., `sbtn testCompilation i12345`)
- `sbtn scala3-bootstrapped/test` - Run all tests (slow, CI-level)
- `sbtn "testCompilation --update-checkfiles"` - Update `.check` files with actual output


### REPL and Tools
- `sbtn repl` - Start the Scala 3 REPL
- `sbtn scala3-bootstrapped/publishLocal` - Build compiler locally for testing in other projects
- `sbtn dist/Universal/packageBin` - Build a distribution (output in `dist/target/`)

## Test Directory Structure

Tests are in `tests/` with categories:
- `tests/pos` - Should compile successfully
- `tests/neg` - Should fail to compile (requires `// error` annotations on expected error lines)
- `tests/run` - Compile and execute (requires `@main def Test` or `object Test extends App`)
- `tests/pos-custom-args`, `tests/neg-custom-args` - Tests requiring special compiler options

Custom compiler options can be specified:
1. Per-directory in `compiler/test/dotty/tools/dotc/CompilationTests.scala`
2. Per-file with `//> using options <flags>` directive at the top of the file

Capture checking tests are in `tests/{pos,neg}-custom-args/captures`. Use `-Ycc-verbose` for verbose capture checking output.

Multi-file tests: Use `_1.scala`, `_2.scala` suffixes for ordered separate compilation, or no suffix for joint compilation.

Checkfiles: `<test>.check` files contain expected compiler output or program output.

Test annotations (magic comments):
- `// error` - Expect an error on this line (use multiple times for multiple errors)
- `// warn` - Expect a warning on this line
- `// nopos-error` - Expect an error with no position (can be anywhere in file)
- `// anypos-error` - Expect an error with position that can't be annotated
- `// scalajs: --skip` - Skip this test for Scala.js

## Compiler Architecture

### Source Layout
```
compiler/src/dotty/tools/
├── dotc/           # Main compiler
│   ├── ast/        # Abstract syntax trees
│   ├── core/       # Core data structures (Types, Symbols, Contexts, etc.)
│   ├── parsing/    # Scanner and parser
│   ├── typer/      # Type checking (namer, typer)
│   ├── transform/  # Tree transformation phases
│   ├── reporting/  # Error messages and diagnostics
│   └── cc/         # Capture checking (experimental)
├── backend/        # JVM and JS code generation
└── io/             # File and classpath handling
```

### Compiler Phases

Phases are grouped into: `frontendPhases` → `picklerPhases` → `transformPhases` → `backendPhases`

Key phases:
1. **Parser** - Source to untyped AST
2. **TyperPhase** - Type checking, produces typed AST
3. **PostTyper** - Additional checks after typing
4. **Pickler** - Generates TASTY (serialized typed AST)
5. **Inlining** - Inline expansion and macro execution
6. **Erasure** - Erases types to JVM model
7. **GenBCode** - JVM bytecode generation

### Core Data Structures

Located in `compiler/src/dotty/tools/dotc/core/`:
- **Types.scala** - Type representation hierarchy (`TypeRef`, `TermRef`, `MethodType`, etc.)
- **Symbols.scala** - Symbol table entries
- **Contexts.scala** - Compiler context (implicit parameter threading state through compilation)
- **Denotations.scala** - Symbol meanings at specific phases
- **Flags.scala** - Symbol flags (abstract, final, private, etc.)
- **Names.scala** - Interned name representation

### Key Conventions

- Almost all methods take an implicit `Context` parameter named `ctx`
- Phases are "miniphases" fused into single tree traversals for efficiency
- Types use proxy pattern: `TypeProxy` wraps another type via `underlying`

## Bootstrapping

The compiler has two configurations:
- **nonbootstrapped** - Built with a released Scala 3 compiler (faster iteration)
- **bootstrapped** - Built with itself (required for some tests, final validation)

Use `scala3-compiler-bootstrapped/testCompilation` for tests requiring the bootstrapped compiler.

## Documentation

Documentation lives in `docs/_docs/`:
- `docs/_docs/reference/` - Scala 3 language reference
- `docs/_docs/reference/experimental/capture-checking/` - Capture checking specification
- `docs/_docs/internals/` - Compiler internals documentation
