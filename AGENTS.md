# AGENTS.md

This file provides guidance to AI coding agents when working with code in this repository.

**Important**:
- All AI-assisted contributions, including  code, PR/issue descriptions, comments, and code reviews, must comply with the [LLM usage policy](LLM_POLICY.md) . **Important**: state LLM usage clearly in descriptions, commit messages, etc.
- Read `CONTRIBUTING.md`, in particular the "Forbidden" section!

## Project Overview

Dotty is the Scala 3 compiler (`dotc`). It compiles Scala source code into JVM bytecode (and optionally Scala.js IR). The compiler is itself written in Scala 3.

We care about maintainability. "The tests pass" is not enough to justify that a change is good.

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

## Commit and PR Format

`CONTRIBUTING.md`, `LLM_POLICY.md`, and `.github/pull_request_template.md` are authoritative. If this section conflicts with them, follow them and update this file.

### Scope

- One change per PR. Do not mix behavior changes, formatting churn, dependency bumps, and unrelated cleanup.
- An issue should generally exist before opening a PR (see `CONTRIBUTING.md`); short, self-contained changes (e.g. typo fixes) may skip this.
- Do not revert unrelated local changes (e.g. `community-build/` submodule pointers); stage only the files your change touches.

### Commit message

Use a short imperative subject (≤72 chars). For non-trivial commits, use this body:

```text
<short imperative subject>

Motivation:
Why this change is needed. Link the issue when applicable.

Modification:
What was changed.

Result:
Observable outcome or behavior change.

Tests:
- <command / result>, e.g. `sbt --client testCompilation i12345`
- or `Not run - docs only`

References:
Fixes #1234, Refs #1234, or `None - <short context>`
```

- Never omit `Tests`. Use `Not run - docs only` for documentation-only commits. Record any required tool that was missing (e.g. `scalafmt: not installed - skipped`).
- Never omit `References`. Use `None - <short context>` when there is no related issue.
- Trivial commits (typo fix, doc rewording) may use a single-line subject plus a short `References` footer.
- Do not add `Co-authored-by` or AI-assistant trailers; declare LLM usage in the PR body per `LLM_POLICY.md`.

### Pull request

Follow `.github/pull_request_template.md` verbatim. In particular:

- Start the PR description with `Fixes #XYZ`, or explain why no issue exists.
- Pick exactly one LLM-usage line (`Extensively`, `Moderately`, `Minimally`, `Not at all`) per `LLM_POLICY.md`.
- Pick exactly one testing line (new tests, covered by existing tests, non-code, or manual with detail).
- Bug-fix PRs should add a regression test that fails before the fix.
- Behavior, public API, language, or operator-visible changes must update the matching docs under `docs/_docs/`.
- Scala and Java DSL changes must keep API, docs, and tests in parity.
- Run `git diff --check` and `sbt --client testCompilation` (or the smallest relevant test) before marking the PR ready.
- Do not weaken assertions or raise timeouts to silence a flake; encode the intended ordering or contract instead.
