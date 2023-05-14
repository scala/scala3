---
layout: doc-page
title: "Dotc's concept of time"
---

Conceptually, the `scalac` compiler's job is to maintain views of various
artifacts associated with source code at all points in time.  But what is
*time* for `scalac`? In fact, it is a combination of compiler runs and compiler
phases.

The *hours* of the compiler's clocks are measured in compiler [runs]. Every run
creates a new hour, which follows all the compiler runs (hours) that happened
before. `scalac` is designed to be used as an incremental compiler that can
support incremental builds, as well as interactions in an IDE and a REPL. This
means that new runs can occur quite frequently.  At the extreme, every
keystroke in an editor or REPL can potentially launch a new compiler run, so
potentially an "hour" of compiler time might take only a fraction of a second
in real time.

The *minutes* of the compiler's clocks are measured in phases. At every
compiler run, the compiler cycles through a number of [phases]. The list of
phases is defined in the [Compiler]object There are currently about 60 phases
per run, so the minutes/hours analogy works out roughly. After every phase the
view the compiler has of the world changes: trees are transformed,  types are
gradually simplified from Scala types to JVM types, definitions are rearranged,
and so on.

Many pieces in the information compiler are time-dependent. For instance, a
Scala symbol representing a definition has a type, but that type will usually
change as one goes from the higher-level Scala view of things to the
lower-level JVM view. There are different ways to deal with this. Many
compilers change the type of a symbol destructively according to the "current
phase". Another, more functional approach might be to have different symbols
representing the same definition at different phases, which each symbol
carrying a different immutable type. `scalac` employs yet another scheme, which
is inspired by functional reactive programming (FRP): Symbols carry not a
single type, but a function from compiler phase to type. So the type of a
symbol is a time-indexed function, where time ranges over compiler phases.

Typically, the definition of a symbol or other quantity remains stable for a
number of phases. This leads us to the concept of a [period]. Conceptually,
period is an interval of some given phases in a given compiler run. Periods
are conceptually represented by three pieces of information

* the ID of the current run,
* the ID of the phase starting the period
* the number of phases in the period

All three pieces of information are encoded in a value class over a 32 bit
integer. Here's the API for class `Period`:

```scala
class Period(val code: Int) extends AnyVal {
  def runId: RunId            // The run identifier of this period.
  def firstPhaseId: PhaseId   // The first phase of this period
  def lastPhaseId: PhaseId    // The last phase of this period
  def phaseId: PhaseId        // The phase identifier of this single-phase period

  def containsPhaseId(id: PhaseId): Boolean
  def contains(that: Period): Boolean
  def overlaps(that: Period): Boolean

  def & (that: Period): Period
  def | (that: Period): Period
}
```

We can access the parts of a period using `runId`, `firstPhaseId`,
`lastPhaseId`, or using `phaseId` for periods consisting only of a single
phase. They return `RunId` or `PhaseId` values, which are aliases of `Int`.
`containsPhaseId`, `contains` and `overlaps` test whether a period contains a
phase or a period as a sub-interval, or whether the interval overlaps with
another period. Finally, `&` and `|` produce the intersection and the union of
two period intervals (the union operation `|` takes as `runId` the `runId` of
its left operand, as periods spanning different `runId`s cannot be constructed.

Periods are constructed using two `apply` methods:

```scala
object Period {
  /** The single-phase period consisting of given run id and phase id */
  def apply(rid: RunId, pid: PhaseId): Period

  /** The period consisting of given run id, and lo/hi phase ids */
  def apply(rid: RunId, loPid: PhaseId, hiPid: PhaseId): Period
}
```

As a sentinel value there's `Nowhere`, a period that is empty.

[runs]: https://github.com/lampepfl/dotty/blob/a527f3b1e49c0d48148ccfb2eb52e3302fc4a349/compiler/src/dotty/tools/dotc/Run.scala
[phases]: https://github.com/lampepfl/dotty/blob/a527f3b1e49c0d48148ccfb2eb52e3302fc4a349/compiler/src/dotty/tools/dotc/core/Phases.scala
[period]: https://github.com/lampepfl/dotty/blob/a527f3b1e49c0d48148ccfb2eb52e3302fc4a349/compiler/src/dotty/tools/dotc/core/Periods.scala
