---
layout: doc-page
title: Dotty Internals 1: Trees & Symbols (Meeting Notes)
---

These are meeting notes for the [Dotty Internals 1: Trees & Symbols](https://www.youtube.com/watch?v=yYd-zuDd3S8) talk by [Dmitry Petrashko](http://twitter.com/darkdimius) on Mar 21, 2017.

# Entry point
`dotc/Compiler.scala`

The entry point to the compiler contains the list of phases and their order.

# Phases

Some phases executed independently, but others (miniphases) are grouped for efficiency.
See the paper "[Miniphases: Compilation using Modular and Efficient Tree Transformation](https://infoscience.epfl.ch/record/228518/files/paper.pdf)" for details.

# Trees
`dotc/ast/Trees.scala`

Trees represent code written by the user (e.g. methods, classes, expressions). There are two kinds of trees: untyped and typed.

Unlike other compilers (but like `scalac`), dotty doesn't use multiple intermediate representations (IRs) during the compilation pipeline. Instead, it uses trees for all phases.

Dotty trees are immutable, so they can be shared.

## Untyped trees
`dotc/ast/untpd.scala`

These are the trees as output by the parser.

Some trees only exist as untyped: e.g. `WhileDo` and `ForDo`. These are desugared by the typechecker.

## Typed trees
`dotc/ast/tpd.scala`

Typed trees contain not only the user-written code, but also semantic information (the types) about the code.

## Notes on some tree types

  * `RefTree`: trees that refer to something. There are multiple subtypes
    - `Ident`: by-name reference
    - `Select`: select (e.g. a field) from another tree (e.g. `a.foo` is represented as `Select(Ident(a), foo)`)
  * `This`: the this pointer
  * `Apply`: function application: e.g. `a.foo(1, 2)(3, 4)` becomes `Apply(Apply(Select(Ident(a), foo), List(1, 2)), List(3, 4))`
  * `TypeApply`: type application: `def foo[T](a: T) = ??? foo[Int](1)` becomes `Apply(TypeApply(Ident(foo), List(Int)), List(1))`
  * `Literal`: constants (e.g. integer constant 1)
  * `Typed`: type ascription (e.g. for widening, as in `(1: Any)`)
  * `NamedArg`: named arguments (can appear out-of-order in untyped trees, but will appear in-order in typed ones)
  * `Assign`: assignment. The node has a `lhs` and a `rhs`, but the `lhs` can be arbitrarily complicated (e.g. `(new C).f = 0`).
  * `If`: the condition in an if-expression can be arbitrarily complex (e.g. it can contain class definitions)
  * `Closure`: the free variables are stored in the `env` field, but are only accessible "around" the `LambdaLift` phase.
  * `Match` and `CaseDef`: pattern-matching trees. The `pat` field in `CaseDef` (the pattern) is, in turn, populated with a subset of trees like `Bind` and `Unapply`.
  * `Return`: return from a method. If the `from` field is empty, then we return from the closest enclosing method.
     The `expr` field should have a types that matches the return type of the method, but the `Return` node itself has type bottom.
  * `TypeTree`: tree representing a type (e.g. for `TypeApply`).
  * `AndType`, `OrType`, etc.: these are other trees that represent types that can be written by the user. These are a strict subset of all types, since
    some types *cannot* be written by the user.
  * `ValDef`: defines fields or local variables. To differentiate between the two cases, we can look at the denotation.
    The `preRhs` field is lazy because sometimes we want to "load" a definition without know what's on the rhs (for example, to look up its type).
  * `DefDef`: method definition.
  * `TypeDef`: type definition. Both `type A = ???` and `class A {}` are represented with a `TypeDef`. To differentiate between the two, look at the type of the node (better), or in the case of classes there should be a `Template` node in the rhs.
  * `Template`: describes the "body" of a class, including inheritance information and constructor. The `constr` field will be populated only after the `Constructors` phase; before that the constructor lives in the `preBody` field.
  * `Thicket`: allows us to return multiple trees when a single one is expected. This kind of tree is not user-visible.
    For example, `transformDefDef` in `LabelDefs` takes in a `DefDef` and needs to be able to sometimes break up the method into multiple methods, which are then returned as a single tree (via a `Thicket`). If we return a thicket in a location where multiple trees are expected, the compiler will flatten them, but if only one tree is expected (for example, in the constructor field of a class), then the compiler will throw.

### ThisTree

Tree classes have a `ThisTree` type field which is used to implement functionality that's common for *all* trees while returning
a specific tree type. See `withType` in the `Tree` base class, for an example.

Additionally, both `Tree` and `ThisTree` are polymorphic so they can represent both untyped and typed trees.

For example, `withType` has signature `def withType(tpe: Type)(implicit ctx: Context): ThisTree[Type]`.
This means that `withType` can return the most-specific tree type for the current tree, while at the same time guaranteeing that
the returned tree will be typed.

## Creating trees

You should use the creation methods in `untpd.scala` and `tpd.scala` to instantiate tree objects (as opposed to
creating them directly using the case classes in `Trees.scala`).

## Meaning of trees

In general, the best way to know what a tree represents is to look at its type or denotation; pattern matching
on the structure of a tree is error-prone.

## Errors
`dotc/typer/ErrorReporting.scala`

Sometimes there's an error during compilation, but we want to continue compiling (as opposed to failing outright), to
uncover additional errors.

In cases where a tree is expected but there's an error, we can use the `errorTree` methods in `ErrorReporting` to create
placeholder trees that explicitly mark the presence of errors.

Similarly, there exist `ErrorType` and `ErrorSymbol` classes.

## Assignment

The closest in Dotty to what a programming language like C calls an "l-value" is a `RefTree` (so an `Ident` or a `Select`).
However, keep in mind that arbitrarily complex expressions can appear in the lhs of an assignment: e.g.
```scala
trait T {
  var s = 0
}
{
  class T2 extends T
  while (true) 1
  new Bla
}.s = 10
```
Another caveat, before typechecking there can be some trees where the lhs isn't a `RefTree`: e.g. `(a, b) = (3, 4)`.

# Symbols
`dotc/core/Symbols.scala`

Symbols are references to definitions (e.g. of variables, fields, classes). Symbols can be used to refer to definitions for which we don't have ASTs (for example, from the Java standard library).

`NoSymbol` is used to indicate the lack of a symbol.

Symbols uniquely identify definitions, but they don't say what the definitions *mean*. To understand the meaning of a symbol
we need to look at its *denotation* (spefically for symbols, a `SymDenotation`).

Symbols can not only represent terms, but also types (hence the `isTerm`/`isType` methods in the `Symbol` class).

## ClassSymbol

`ClassSymbol` represents either a `class`, or an `trait`, or an `object`. For example, an object
```scala
object O {
  val s = 1
}
```
is represented (after `Typer`) as
```scala
class O$ { this: O.type =>
  val s = 1
}
val O = new O$
```
where we have a type symbol for `class O$` and a term symbol for `val O`. Notice the use of the selftype `O.type` to indicate that `this` has a singleton type.

## SymDenotation
`dotc/core/SymDenotations.scala`

Symbols contain `SymDenotation`s. The denotation, in turn, refers to:

  * the source symbol (so the linkage is cyclic)
  * the "owner" of the symbol:
    - if the symbol is a variable, the owner is the enclosing method
    - if it's a field, the owner is the enclosing class
    - if it's a class, then the owner is the enclosing class
  * a set of flags that contain semantic information about the definition (e.g. whether it's a trait or mutable). Flags are defined in `Flags.scala`.
  * the type of the definition (through the `info` method)
