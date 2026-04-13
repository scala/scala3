# AGENTS.md

This file provides guidance to AI coding agents when working with code in this repository.

## Project Overview

Dotty is the Scala 3 compiler (`dotc`). It compiles Scala source code into JVM bytecode (and optionally Scala.js IR). The compiler is itself written in Scala 3.

## Build Commands

Use `sbt --client` (sbt with native thin client) to run commands. This keeps an sbt server running for faster subsequent invocations.

### Compilation
- `sbt --client scalac <file>` - Compile a file with the current compiler changes
- `sbt --client scala <class>` - Run a compiled class

Use the `local/` directory for temporary test files (gitignored).

### Testing
- `sbt --client testCompilation` - Run the whole compilation test suite.
- `sbt --client testCompilation <name>` - Run compilation tests matching `<name>` (e.g., `sbt --client testCompilation i12345`)
- `sbt --client scala3-bootstrapped/test` - Run all tests (slow, CI-level)
- `sbt --client "testCompilation --update-checkfiles"` - Update `.check` files with actual output


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

For available AI-assisted documentation workflows or skills start refer to [./docs/.ai/INDEX.md](./docs/.ai/INDEX.md)
