# mill-scala3-bench

Two Mill single-file Scala scripts (`bench.scala`, `profile.scala`) that
measure / profile how long the local `scala3-compiler-nonbootstrapped`
takes to compile Mill's `libs.javalib` module graph. Both drive the same
Scala runner (`src/ProfileRunner.scala`), which loops the compiler N
times in one JVM and prints `[profile-runner] run i/N: X ms` per iter.
Both scripts run a one-time corpus init from Maven Central automatically
on first use.

## Layout

| path | role |
|---|---|
| `../mill` | Mill 1.1.6 bootstrap script (at the repo root) |
| `Common.scala` | shared library: sbt + classpath, ProfileRunner build/run, JFR analysis (binary API), summary aggregation, Mill corpus init |
| `bench.scala` | `def main` entry — N back-to-back compiles, prints mean/median/stddev |
| `profile.scala` | `def main` entry — record + analyze JFR(s), write `profile-summary.txt`; with `--baseline` / `--opt` writes into an auto-inferred `iter-N/{baseline,opt-M}/` dir |
| `src/ProfileRunner.scala` | the runner — N compiles in one JVM, prints per-run ms |

Generated state (gitignored) lives under `target/bench-mill-javalib/`:
`inputs/` (text files), `sources/` (extracted .scala/.java),
`target/build/` (JSON, logs, JFR).

Mill's own scratch lives under `bench-mill-javalib/out/` (also gitignored).

## Usage

Mainargs maps each `def main` parameter to a kebab-cased CLI flag.

Invoke from the repo root:

```
./mill bench-mill-javalib/profile.scala              # 5 JFR JVMs x 10 runs into auto-allocated iter-N/run-M/
./mill bench-mill-javalib/profile.scala --new-iter   # ... but start a fresh iter-N (run-0 inside it)
./mill bench-mill-javalib/profile.scala --jfr-out PATH        # explicit output path
./mill bench-mill-javalib/profile.scala --analyze-only PATH   # re-aggregate existing JFRs
./mill bench-mill-javalib/bench.scala --name NAME    # build + N compiles + mean/median/stddev
./mill bench-mill-javalib/bench.scala --analyze-only PATH
```

Corpus init runs automatically on first use (or pass `--force-setup` to
redo it). Each `profile.scala` invocation without `--jfr-out` / `--analyze-only`
allocates the next `iter-N/run-M/` slot under `target/bench-mill-javalib/` —
extending the latest `iter-N/` by default, or starting a fresh one with `--new-iter`.
`target/bench-mill-javalib/` is anchored to the main worktree (via `git
rev-parse --git-common-dir`), so running from any linked worktree writes
to the same shared location.

The local non-bootstrapped compiler is built via `sbt
scala3-compiler-nonbootstrapped/compile`. The compile output is scraped
for the `compiling N Scala source(s)` line and the run errors out if it's
absent — this catches the case where Zinc thought nothing changed after a
cross-commit `git checkout`.

### `bench.scala`

Runs the compiler `--runs N` times (default 20) in one JVM, parses the
runner's per-iter timing lines, then prints a one-line iter table plus
mean/median/stddev for the full set and the `--drop-first N` (default 10)
warm tail.

The raw timings are persisted as JSON via upickle in
`target/bench-mill-javalib/target/build/bench-NAME.json`. Pass
`--analyze-only PATH` to re-process a saved JSON without re-running.

### `profile.scala`

Runs the loop (default `--runs 10`) under JFR (`stackdepth=256`,
`ExecutionSample`@1ms, TLAB allocation events on with stack traces). Pass
`--repeat N` to run the cycle N times in separate JVMs (default 5).

JFR analysis uses `jdk.jfr.consumer.RecordingFile` (binary API) — ~10x
faster than `jfr print` + regex parsing. Per-method self%/tot% and
per-class allocation share%/bytes are aggregated across runs as mean ±
sample std dev into `<stem>-summary.txt`. Filter by `--thread NAME`
(default `main`).

`--analyze-only PATH` skips recording and just re-aggregates the existing
`<stem>-run{1..N}.jfr` files alongside `PATH`.
