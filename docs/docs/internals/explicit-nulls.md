---
layout: doc-page
title: "Explicit Nulls"
---

The "explicit nulls" feature (enabled via a flag) changes the Scala type hierarchy
so that reference types (e.g. `String`) are non-nullable. We can still express nullability
with union types: e.g. `val x: String|Null = null`.

The implementation of the feature in dotty can be conceptually divided in several parts:
  1. changes to the type hierarchy so that `Null` is only a subtype of `Any`
  2. a "translation layer" for Java interop that exposes the nullability in Java APIs
  3. a "magic" `JavaNull` type (an alias for `Null`) that is recognized by the compiler and
     allows unsound member selections (trading soundness for usability)
  4. a module for "flow typing", so we can work more naturally with nullable values

Feature Flag
------------
Explicit nulls are disabled by default. They can be enabled via `-Yexplicit-nulls` defined in
`ScalaSettings.scala`. All of the explicit-nulls-related changes should be gated behind the flag.

Type Hierarchy
--------------
We change the type hierarchy so that `Null` is only a subtype of `Any` by:
  - modifying the notion of what is a nullable class (`isNullableClass`) in `SymDenotations`
    to include _only_ `Null` and `Any`
  - changing the parent of `Null` in `Definitions` to point to `Any` and not `AnyRef`
  - changing `isBottomType` and `isBottomClass` in `Definitions`

Java Interop
------------
TODO(abeln): add support for recognizing nullability annotations a la 
https://kotlinlang.org/docs/reference/java-interop.html#nullability-annotations

The problem we're trying to solve here is: if we see a Java method `String foo(String)`,
what should that method look like to Scala?
  - since we should be able to pass `null` into Java methods, the argument type should be `String|JavaNull`
  - since Java methods might return `null`, the return type should be `String|JavaNull`

`JavaNull` here is a type alias for `Null` with "magic" properties (see below).

At a high-level:
  - we track the loading of Java fields and methods as they're loaded by the compiler
  - we do this in two places: `Namer` (for Java sources) and `ClassFileParser` (for bytecode)
  - whenever we load a Java member, we "nullify" its argument and return types

The nullification logic lives in `JavaNullInterop.scala`, a new file.

The entry point is the function `def nullifyMember(sym: Symbol, tp: Type)(implicit ctx: Context): Type`
which, given a symbol and its "regular" type, produces what the type of the symbol should be in the
explicit nulls world.

In order to nullify a member, we first pass it through a "whitelist" of symbols that need
special handling (e.g. `constructors`, which never return `null`). If none of the "policies" in the
whitelist apply, we then process the symbol with a `TypeMap` that implements the following nullification
function `n`:
  1. n(T)              = T|JavaNull              if T is a reference type
  2. n(T)              = T                       if T is a value type
  3. n(T)              = T|JavaNull              if T is a type parameter
  4. n(C[T])           = C[T]|JavaNull           if C is Java-defined
  5. n(C[T])           = C[n(T)]|JavaNull        if C is Scala-defined
  6. n(A|B)            = n(A)|n(B)|JavaNull
  7. n(A&B)            = (n(A)&n(B))|JavaNull
  8. n((A1, ..., Am)R) = (n(A1), ..., n(Am))n(R) for a method with arguments (A1, ..., Am) and return type R
  9. n(T)              = T                       otherwise

JavaNull
--------
`JavaNull` is just an alias for `Null`, but with magic power. `JavaNull`'s magic (anti-)power is that
it's unsound.

```scala
val s: String|JavaNull = "hello"
s.length // allowed, but might throw NPE
```

`JavaNull` is defined as `JavaNullAlias` in `Definitions`.
The logic to allow member selections is defined in `findMember` in `Types.scala`:
  - if we're finding a member in a type union
  - and the union contains `JavaNull` on the r.h.s. after normalization (see below)
  - then we can continue with `findMember` on the l.h.s of the union (as opposed to failing)

Working with Nullable Unions
----------------------------
Within `Types.scala`, we defined a few utility methods to work with nullable unions. All of these
are methods of the `Type` class, so call them with `this` as a receiver:
  - `isNullableUnion` determines whether `this` is a nullable union. Here, what constitutes
     a nullable union is determined purely syntactically:
       1. first we "normalize" `this` (see below)
       2. if the result is of the form `T | Null`, then the type is considered a nullable union.
          Otherwise, it isn't.
  - `isJavaNullableUnion` determines whether `this` is syntactically a union of the form `T|JavaNull`
  - `normNullableUnion` normalizes `this` as follows:
      1. if `this` is not a nullable union, it's returned unchanged.
      2. if `this` is a union, then it's re-arranged so that all the `Null`s are to the right of all
         the non-`Null`s.
  - `stripNull` syntactically strips nullability from `this`: e.g. `String|Null => String`. Notice this
     works only at the "top level": e.g. if we have an `Array[String|Null]|Null` and we call `stripNull`
     we'll get `Array[String|Null]` (only the outermost nullable union was removed).
  - `stripAllJavaNull` is like `stripNull` but removes _all_ nullable unions in the type (and only works
     for `JavaNull`). This is needed when we want to "revert" the Java nullification function.

Flow Typing
-----------
Flow typing is needed so we can work with nullable unions in a more natural way.
The following is a common idiom that should work without additional casts:
```scala
val x: String|Null = ???
if (x != null && x.length < 10)
```
This is implemented as a "must be null in the current scope" analysis on stable paths:
  - we add additional state to the `Context` in `Contexts.scala`.
    Specifically, we add a set of `FlowFacts` (right now just a set of `TermRef`s), which
    are the paths known to be non-nullable in the current scope.
  - the bulk of the flow typing logic lives in a new `FlowTyper.scala` file.
    
    There are four entry points to `FlowTyper`:
      1. `inferFromCond(cond: Tree): Inferred`: given a tree representing a condition such as
          `x != null && x.length < 10`, return the `Inferred` facts.
           
          In turn, `Inferred` is defined as `case class Inferred(ifTrue: FlowFacts, ifFalse: FlowFacts)`.
          That is, `Inferred` contains the paths that _must_ be non-null if the condition is true and,
          separately, the paths that must be non-null if the condition is false.

          e.g. for `x != null` we'd get `Inferred({x}, {})`, but only if `x` is stable.
          However, if we had `x == null` we'd get `Inferred({}, {x})`.
      	  
      2. `inferWithinCond(cond: Tree): FlowFacts`: given a condition of the form `lhs && rhs` or
         `lhs || rhs`, calculate the paths that must be non-null for the rhs to execute (given
         that these operations) are short-circuiting.

      3. `inferWithinBlock(stat: Tree): FlowFacts`: if `stat` is a statement with a block, calculate
         which paths must be non-null when the statement that _follows_ `stat` in the block executes.
         This is so we can handle things like
         ```scala
         val x: String|Null = ???
	     if (x == null) return
         val y = x.length
         ```
         Here, `inferWithinBlock(if (x == null) return)` gives back `{x}`, because we can tell that
         the next statement will execute only if `x` is non-null.

      4. `refineType(tpe: Type): Type`: given a type, refine it if possible using flow-sensitive type
         information. This uses a `NonNullTermRef` (see below).

  - Each of the public APIs in `FlowTyper` is used to do flow typing in a different scenario
    (but all the use sites of `FlowTyper` are in `Typer.scala`):
    * `refineType` is used in `typedIdent` and `typedSelect`
    * `inferFromCond` is used for typing if statements
    * `inferWithinCond` is used when typing "applications" (which is how "&&" and "||" are encoded)
    * `inferWithinBlock` is used when typing blocks

    For example, to do FlowTyping on if expressions:
      * we type the condition
      * we give the typed condition to the FlowTyper and obtain a pair of sets of paths `(ifTrue, ifFalse)`.
        We type the `then` branch with the `ifTrue` facts, and the else branch with the `ifFalse` facts.
      * profit

Flow typing also introduces two new abstractions: `NonNullTermRef` and `ValDefInBlockCompleter`.

#### NonNullTermRef
This is a new type of `TermRef` (path-dependent type) that, whenever its denotation is updated, makes sure
that the underlying widened type is non-null. It's defined in `Types.scala`. A `NonNullTermRef` is identified by `computeDenot` whenever the denotation is updated, and then we call `stripNull` on the widened type.

To use the flow-typing information, whenever we see a path that we know must be non-null (in `typedIdent` or
`typedSelect`), we replace its `TermRef` by a `NonNullTermRef`.

#### ValDefInBlockCompleter
This a new type of completer defined in `Namer.scala` that completes itself using the completion context, asopposed to the creation context.

The problem we're trying to solve here is the following:
```scala
val x: String|Null = ???
if (x == null) return
val y = x.length
```
The block is usually typed as follows:
  1. first, we scan the block to create symbols for the new definitions (`val x`, `val y`)
  2. then, we type statement by statement
  3. the completers for the symbols created in 1. are _all_ invoked in step 2. However,
     regular completers  use the _creation_ context, so that means that `val y` is completed
     with a context that doesn't contain the new flow fact "x != null".

To fix this, whenever we're inside a block and we create completers for `val`s, we use a 
`ValDefInBlockCompleter` instead of a regular completer. This new completer uses the completion context,
which is aware of the new flow fact "x != null".
