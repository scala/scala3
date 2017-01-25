# Benchmark results for implicit compilation scenarios

### Setup

Three alternatives:

  1. No implicit shortcuts
  2. Implicit shortcuts only for possible targets of megamorphic dispatch
     (`specializeMonoTargets` in [ShortcutImplicits.scala](../../../compiler/src/dotty/tools/dotc/transform/ShortcutImplicits.scala) set to false)
  3. Implicit shortcuts for all methods returning implicit function types
     (`specializeMonoTargets` set to true)

Two benchmarks:

 - `ImplicitMono`:  All calls are monomorphic.
                    Code in [ImplicitMono.scala](./ImplicitMono.scala).
 - `ImplicitMega` : About half of the calls are (4-way) megamorphic,
                    the others are monomorphic.
                    Code in [ImplicitMega.scala](./ImplicitMega.scala).

### Results

| Scheme              | ImplicitMono | ImplicitMega |
|---------------------|-------------:|-------------:|
| no shortcuts        | 1354ms       | 3260ms
|                     | 955ms        | 2906ms
|                     | 908ms        | 2899ms
|                     | 906ms        | 2887ms
|                     | 886ms        | 2872ms
| only mega shortcuts |        |
                      | 1243ms | 2472ms
|                     | 926ms  | 2146ms
|                     | 925ms  | 2169ms
|                     | 902ms  | 2136ms
|                     | 913ms  | 2179ms
| all shortcuts       |        |
|                     | 1354ms | 1940ms
|                     | 1039ms | 1563ms
|                     | 1031ms | 1593ms
|                     | 1065ms | 1548ms
|                     | 1016ms | 1579ms

### Interpretation

In the fully monomorphic benchmark, specializing
only megamorphic targets has the same performance as
not specializing at all (not surprising, since there
are no megamorphic targets). Specializing everything
incurs about a 14% performance hit (maybe due to the extra
code generated; it's hard to pin down what it is).

Note: We compute relative performance differences by comparing the
second-best test runs of each series with each other.

In the megamorphic benchmark, it's the other way round.
Specializing only megamorphic call-sites leads to a performance
improvement of about 36% compared to no specialization. Specializing
everything leads to another 37% improvement (85% total compared
to no specialization).

I think we need larger benchmarks to decide whether we should
specialize monomorphic call-targets or not.

### Comparing with the Reader Monad

Translating `ImplicitMega` to the reader monad (code in [ReaderMonadic.scala](./ReaderMonadic.scala)) gives the following runtimes:

| Reader |
|---------|
| 11563ms |
| 11108ms |
| 11300ms |
| 11098ms |
| 11159ms |

This translates to a 710% slowdown compared to implicit function types
with full specialization.