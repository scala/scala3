---
layout: doc-page
title: Symbols
---

As discussed previously, `dotc` [maintains time-indexed views](time.md) of various
compiler artifacts. The following sections discuss how they are managed in the compiler.

## Symbols

Defined in [Symbols], a `Symbol` is a unique identifier for a definition (e.g. a method,
type, or field).  A `ClassSymbol` extends `Symbol` and represents either a
`class`, or a `trait`, or an `object`. A `Symbol` can even refer to non-Scala entities,
such as from the Java standard library.

## Definitions are Dynamic

Traditionally, compilers store context-dependent data in a _symbol table_.
Where a symbol then is the central reference to address context-dependent data.
`dotc` instead uses a phase-indexed function (known as
a [Denotation][Denotations]) to compute views of definitions across phases,
as many of attributes associated with definitions are phase-dependent. For example:
- types are gradually simplified by several phases,
- owners change in [lambdaLift] (local methods are lifted to an enclosing class)
  and [flatten] (when inner classes are moved to the top level)
- Names are changed when private members need to be accessed from outside
  their class (for instance from a nested class or a class implementing
  a trait).

Additionally, symbols are not suitable to be used as a reference to
a definition in another [compilation unit][CompilationUnit].
In the context of incremental compilation, a symbol from
an external compilation unit may be deleted or changed, making the reference
stale. To counter this, `dotc` types trees of cross-module references with either
a `TermRef` or `TypeRef`. A reference type contains a prefix type and a name.
The denotation that the type refers to is established dynamically based on
these fields.

## Denotations

On its own a `Symbol` has no structure. Its semantic meaning is given by being associated
with a [Denotation][Denotations].

A denotation is the result of resolving a name during a given period, containing the information
describing some entity (either a term or type), indexed by phase. Denotations usually have a
reference to a selected symbol, but not always, for example if the denotation is overloaded,
i.e. a `MultiDenotation`.

### SymDenotations
All definition symbols will contain a `SymDenotation`. The denotation, in turn, contains:
- a reverse link to the source symbol
- a reference to the enclosing symbol that defined the source symbol:
  - for a local variable, the enclosing method
  - for a field or class, the enclosing class
- a set of [flags], describing the definition (e.g. whether it's a trait or mutable).
- the type of the definition (through the `info` method)
- a [signature][Signature1], which uniquely identifies overloaded methods (or else `NotAMethod`).
- and more.

A class symbol will instead be associated with a `ClassDenotation`, which extends `SymDenotation`
with some additional fields specific for classes.

[Signature1]: https://github.com/scala/scala3/blob/a527f3b1e49c0d48148ccfb2eb52e3302fc4a349/compiler/src/dotty/tools/dotc/core/Signature.scala#L9-L33
[Symbols]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/core/Symbols.scala
[flatten]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/transform/Flatten.scala
[lambdaLift]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/transform/LambdaLift.scala
[CompilationUnit]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/CompilationUnit.scala
[Denotations]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/core/Denotations.scala
[SymDenotations]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/core/SymDenotations.scala
[flags]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/core/Flags.scala
