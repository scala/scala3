---
layout: doc-page
title: Compiler Phases
---

As described in the [compiler overview](lifecycle.md#phases), `dotc` is divided into a list of [phases][Phase],
specified in the [Compiler] class.

#### Printing the phases of the Compiler

a flattened list of all the phases can be displayed by invoking
the compiler with the `-Xshow-phases` flag:
```
$ scalac -Xshow-phases
```

## Phase Groups

In class [Compiler] you can access the list of phases with the method `phases`:

```scala
def phases: List[List[Phase]] =
  frontendPhases ::: picklerPhases ::: transformPhases ::: backendPhases
```

You can see that phases are actually grouped into sublists, given by the signature
`List[List[Phase]]`; that is, each sublist forms a phase group that is then *fused* into a
single tree traversal when a [Run] is executed.

Phase fusion allows each phase of a group to be small and modular,
(each performing a single function), while reducing the number of tree traversals
and increasing performance.

Phases are able to be grouped together if they inherit from [MiniPhase].

## Phase Categories

Phases fall into four categories, allowing customisation by sub-classes of [Compiler]:

### `frontendPhases`
In the main compiler these include [parser], [typer], [posttyper],
[prepjsinterop] and phases for producing SemanticDB and communicating with the
incremental compiler Zinc.
The [parser] reads source programs and generates untyped abstract syntax trees, which
in [typer] are then typechecked and transformed into typed abstract syntax trees.
Following is [posttyper], performing checks and cleanups that require a fully typed program.
In particular, it
- creates super accessors representing `super` calls in traits
- creates implementations of compiler-implemented methods,
such as `equals` and `hashCode` for case classes.
- marks [compilation units][CompilationUnit] that require inline expansion, or quote pickling
- simplifies trees of erased definitions
- checks variance of type parameters
- mark parameters passed unchanged from subclass to superclass for later pruning.

### `picklerPhases`
These phases start with [pickler], which serializes typed trees
produced by the `frontendPhases` into TASTy format. Following is [inlining],
which expand calls to inline methods, and [postInlining] providing implementations
of the [Mirror] framework for inlined calls.
Finally are [staging], which ensures that quotes conform to the
[Phase Consistency Principle (PCP)][PCP], and [pickleQuotes] which converts quoted
trees to embedded TASTy strings.

### `transformPhases`
These phases are concerned with transformation into lower-level forms
suitable for the runtime system, with two sub-groupings:
- High-level transformations: All phases from [firstTransform] to [erasure].
  Most of these phases transform syntax trees, expanding high-level constructs
  to more primitive ones.
  - An important transform phase is [patternMatcher], which converts match
    trees and patterns into lower level forms, as well as checking the
    exhaustivity of sealed types, and unreachability of pattern cases.
  - Some phases perform further checks on more primitive trees,
    e.g. [refchecks] verifies that no abstract methods exist in concrete classes,
    and [initChecker] checks that fields are not used before initialisation.
  - The last phase in the group, [erasure] translates all
    types into types supported directly by the JVM. To do this, it performs
    another type checking pass, but using the rules of the JVM's type system
    instead of Scala's.
- Low-level transformations: All phases from `ElimErasedValueType` to
  `CollectSuperCalls`. These further transform trees until they are essentially a
  structured version of Java bytecode.

### `backendPhases`
These map the transformed trees to Java classfiles or SJSIR files.

[CompilationUnit]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/CompilationUnit.scala
[Compiler]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/Compiler.scala
[Phase]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/core/Phases.scala
[MiniPhase]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/transform/MegaPhase.scala
[Run]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/Run.scala
[parser]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/parsing/ParserPhase.scala
[typer]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/typer/TyperPhase.scala
[posttyper]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/transform/PostTyper.scala
[prepjsinterop]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/transform/sjs/PrepJSInterop.scala
[pickler]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/transform/Pickler.scala
[inlining]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/transform/Inlining.scala
[postInlining]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/transform/PostInlining.scala
[staging]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/transform/Staging.scala
[pickleQuotes]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/transform/PickleQuotes.scala
[refchecks]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/typer/RefChecks.scala
[initChecker]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/transform/init/Checker.scala
[firstTransform]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/transform/FirstTransform.scala
[patternMatcher]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/transform/PatternMatcher.scala
[erasure]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/transform/Erasure.scala
[Mirror]: https://github.com/lampepfl/dotty/blob/master/library/src/scala/deriving/Mirror.scala
[PCP]: ../../reference/metaprogramming/macros.md#the-phase-consistency-principle
