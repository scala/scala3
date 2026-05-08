# Scala 3 migration scalafix rules

Mechanical migration rules for Scala 3 stdlib/reflect API tightenings,
written for projects that fail under recent Scala 3 nightlies in the
[VirtusLab Open Community Build](https://virtuslab.github.io/community-build3/).

The compiler's built-in `-rewrite -source X-migration` mechanism handles
syntax-level changes. Library API changes (where the new call shape isn't
mechanically derivable from the old by the compiler alone) are migrated
here via scalafix instead.

## Rules

| Rule | What it does |
|------|--------------|
| `OptionOrNullMigration` | Rewrites `opt.orNull[T]` → `opt.getOrElse(null.asInstanceOf[T])`; lints bare `opt.orNull` (semantics changed to `A \| Null`) |

## Develop

```
sbt ~tests/test
# edit rules/src/main/scala/fix/<rule>.scala
# add input/output cases under input/ and output/
```

## Use locally

Publish the rule artifact and reference it from a target project's
`.scalafix.conf`:

```sh
sbt rules2_13/publishLocal
```

In the target project's `build.sbt`:

```scala
scalafixDependencies += "rules" %% "scala3-migrations" % "0.1.0-SNAPSHOT"
resolvers += Resolver.mavenLocal
semanticdbEnabled := true
semanticdbVersion := scalafixSemanticdb.revision
```

`.scalafix.conf`:

```
rules = [ OptionOrNullMigration ]
```

Run `sbt scalafixAll`.

## End-to-end validation

Each rule was validated against an actual failing project from the
community build:

- **`OptionOrNullMigration`** — applied to `scalikejdbc/scalikejdbc-core`
  call sites (`ResultSetExtractorException.scala:9`,
  `SQL.scala:123`, `StatementExecutor.scala:213`). Rule rewrote all three
  cleanly; rewritten files compile under `3.9.0-RC1-bin-SNAPSHOT` while
  the originals fail with `method orNull in class Option does not take
  type parameters`.

## Future rules

Candidates from the same triage run (see soronpo/dottybug#1):

- `TypedTupleDestructuringSplit` — `val (a: T1 @unchecked, b: T2 @unchecked) = expr`
  → split into separate ascriptions. Hits monix-connect, sttp, dfhdl, scdbpf.
- `SymbolNewModuleParents` — `Symbol.newModule(..., parents: List, ...)` →
  `Symbol.newModule(..., _ => parents, ...)`. Hits fm-serializer.
