---
layout: doc-page
title: "Differences between Scalac and Dotty"
---

Overview explanation how symbols, named types and denotations hang together:
[Denotations1]

## Some background

Dotc is different from most other compilers in that it is centered around the
idea of maintaining views of various artifacts associated with code. These views
are indexed by tne.

A symbol refers to a definition in a source program. Traditionally, compilers
store context-dependent data in a _symbol table_. The symbol then is the central
reference to address context-dependent data. But for `scalac`'s requirements it
turns out that symbols are both too little and too much for this task.

### Too little

The attributes of a symbol depend on the phase. Examples: Types are
gradually simplified by several phases. Owners are changed in phases
`LambdaLift` (when methods are lifted out to an enclosing class) and Flatten
(when all classes are moved to top level). Names are changed when private
members need to be accessed from outside their class (for instance from a nested
class or a class implementing a trait). So a functional compiler, a `Symbol` by
itself met mean much. Instead we are more interested in the attributes of a
symbol at a given phase.

### Too much

If a symbol is used to refer to a definition in another compilation unit, we get
problems for incremental recompilation. The unit containing the symbol might be
changed and recompiled, which might mean that the definition referred to by the
symbol is deleted or changed. This leads to the problem of stale symbols that
refer to definitions that no longer exist in this form. Scala 2 compiler tried
to address this problem by _rebinding_ symbols appearing in certain cross module
references, but it turned out to be too difficult to do this reliably for all
kinds of references. Scala 3 compiler attacks the problem at the root instead.
The fundamental problem is that symbols are too specific to serve as a
cross-module reference in a system with incremental compilation. They refer to a
particular definition, but that definition may not persist unchanged after an
edit.

`scalac` uses instead a different approach: A cross module reference is always
type, either a `TermRef` or ` TypeRef`. A reference type contains a prefix type
and a name. The definition the type refers to is established dynamically based
on these fields.

## Denotation

Comment with a few details: [Denotations2]

A `Denotation` is the result of a name lookup during a given period

* Most properties of symbols are now in the denotation (name, type, owner,
  etc.)
* Denotations usually have a reference to the selected symbol
* Denotations may be overloaded (`MultiDenotation`). In this case the symbol
  may be `NoSymbol` (the two variants have symbols).
* Non-overloaded denotations have an `info`

Denotations of methods have a signature ([Signature1]), which
uniquely identifies overloaded methods.

### Denotation vs. SymDenotation
A `SymDenotation` is an extended denotation that has symbol-specific properties
(that may change over phases)
* `flags`
* `annotations`
* `info`

`SymDenotation` implements lazy types (similar to scalac). The type completer
assigns the denotation's `info`.

### Implicit Conversion
There is an implicit conversion:
```scala
core.Symbols.toDenot(sym: Symbol)(implicit ctx: Context): SymDenotation
```

Because the class `Symbol` is defined in the object `core.Symbols`, the
implicit conversion does **not** need to be imported, it is part of the
implicit scope of the type `Symbol` (check the Scala spec). However, it can
only be applied if an implicit `Context` is in scope.

## Symbol
* `Symbol` instances have a `SymDenotation`
* Most symbol properties in the Scala 2 compiler are now in the denotation (in the Scala 3 compiler).

Most of the `isFooBar` properties in scalac don't exist anymore in dotc. Use
flag tests instead, for example:

```scala
if (sym.isPackageClass)         // Scala 2
if (sym is Flags.PackageClass)  // Scala 3 (*)
```

`(*)` Symbols are implicitly converted to their denotation, see above. Each
`SymDenotation` has flags that can be queried using the `is` method.

## Flags
* Flags are instances of the value class `FlagSet`, which encapsulates a
  `Long`
* Each flag is either valid for types, terms, or both

```
000..0001000..01
        ^     ^^
        flag  | \
              |  valid for term
              valid for type
```

* Example: `Module` is valid for both module values and module classes,
  `ModuleVal` / `ModuleClass` for either of the two.
* `flags.is(Method | Param)`: true if `flags` has either of the two

## Tree
* Trees don't have symbols
  - `tree.symbol` is `tree.denot.symbol`
  - `tree.denot` is `tree.tpe.denot` where the `tpe` is a `NamdedType` (see
    next point)
* Subclasses of `DenotingTree` (`Template`, `ValDef`, `DefDef`, `Select`,
  `Ident`, etc.) have a `NamedType`, which has a `denot` field. The
  denotation has a symbol.
  - The `denot` of a `NamedType` (prefix + name) for the current period is
    obtained from the symbol that the type refers to. This symbol is searched
    using `prefix.member(name)`.

## Type
 * `MethodType(paramSyms, resultType)` from scalac =>
    `mt @ MethodType(paramNames, paramTypes)`. Result type is `mt.resultType`

[Denotations1]: https://github.com/scala/scala3/blob/a527f3b1e49c0d48148ccfb2eb52e3302fc4a349/compiler/src/dotty/tools/dotc/core/Denotations.scala#L27-L72
[Denotations2]: https://github.com/scala/scala3/blob/a527f3b1e49c0d48148ccfb2eb52e3302fc4a349/compiler/src/dotty/tools/dotc/core/Denotations.scala#L77-L103
[Signature1]: https://github.com/scala/scala3/blob/a527f3b1e49c0d48148ccfb2eb52e3302fc4a349/compiler/src/dotty/tools/dotc/core/Signature.scala#L9-L33
