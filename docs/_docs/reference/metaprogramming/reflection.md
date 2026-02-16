---
layout: doc-page
title: "Reflection"
nightlyOf: https://docs.scala-lang.org/scala3/reference/metaprogramming/reflection.html
---

Reflection enables inspection and construction of Typed Abstract Syntax Trees
(Typed-AST).

It may be used on quoted expressions (`quoted.Expr`) and quoted
types (`quoted.Type`) from [Macros](./macros.md) or [multi-staging-programming](./staging.md),
or on whole TASTy files (via [tasty-inspection](./tasty-inspect.md)).
If you are writing macros, please first read [Macros](./macros.md).
You may find all you need without using quote reflection.

## Converting `Expr`s to TASTy reflect trees and back

With `quoted.Expr` and `quoted.Type` we can not only compute code but also analyze code
by inspecting the ASTs. [Macros](./macros.md) provide the guarantee that the
generation of code will be type-correct. Using quote reflection will break these
guarantees and may fail at macro expansion time, hence additional explicit
checks must be done.

To provide reflection capabilities in macros we need to add an implicit parameter
of type `scala.quoted.Quotes` and import `quotes.reflect.*` from it in the scope
where it is used.

```scala
import scala.quoted.*

inline def natConst(inline x: Int): Int = ${natConstImpl('{x})}

def natConstImpl(x: Expr[Int])(using Quotes): Expr[Int] =
  import quotes.reflect.*
  ...
```

We can access the underlying typed AST of an `Expr` using the `asTerm` extension method:

```scala
  val term: Term = x.asTerm
```

Similarly, you can change a `Term` back into an `Expr` with `.asExpr` (returning `Expr[Any]`)
or `.asExprOf[T]` (returning `Expr[T]`, with an exception being thrown at macro-expansion time if the type does not conform).

## Constructing and Analysing trees

Generally, there are 3 main types of constructs you need to know to properly construct and analyse Typed ASTs:
* Trees
* Symbols with Flags
* TypeReprs

### Typed Abstract Syntax Trees
Typed AST is a tree-like representation of the code of a program achieved after typing.
It’s represented by the `Tree` type in the reflection API.

`Terms` are subtypes of trees that represent an expression of certain value. Because of this,
they always have a type associated with them (accessible with `.tpe`). `Terms` can be transformed into `Exprs` with `.asExpr`.

Let’s look at an example in how the `Trees` map into real scala code:

```scala
  val foo: Int = 0
```
The above is represented in the quotes reflect API by a `ValDef` (a subtype of `Tree`, but not `Term`!):
```scala
  ValDef(foo,Ident(Int),Literal(Constant(0))) // ValDef is a subtype of Tree but not Term
```

```scala
  val foo: Int = 0
  foo + 1
```
The above is represented in the quotes reflect API by a `Block` (a subtype of `Term`, itself a subtype of `Tree`)
```scala
  Block(
    List(
      ValDef(foo,Ident(Int),Literal(Constant(0)))
    ),
    Apply(
      Select(Ident(foo),+),
      List(Literal(Constant(1)))
    )
  )
```

You can see the whole hierarchy between different types of Trees in
[`reflectModule` documentation](https://scala-lang.org/api/3.3_LTS/scala/quoted/Quotes$reflectModule.html).

You can also check the shape of code by printing out quoted code transformed into a Term:
```scala
  println( '{ scalaCode }.asTerm )
```
Bear in mind this will always produce a Term. E.g.:
```scala
  '{
    val foo: Int = 0
  }.asTerm
```
Is represented as `Block(List(ValDef(foo,Ident(Int),Literal(Constant(0)))),Literal(Constant(())))`, which is actually a `Block` of `Unit` type:
```scala
  '{
    val foo: Int = 0
    ()
  }
```
#### Tree Extractors and Constructors
`import quotes.reflect.*` provides all extractors, apply-based constructors and methods on `quotes.reflect.Tree`s.
For example, see the `Literal(_)` extractor used below.

```scala
def natConstImpl(x: Expr[Int])(using Quotes): Expr[Int] =
  import quotes.reflect.*
  val tree: Term = x.asTerm
  tree match
    case Inlined(_, _, Literal(IntConstant(n))) =>
      if n <= 0 then
        report.error("Parameter must be natural number")
        '{0}
      else
        tree.asExprOf[Int]
    case _ =>
      report.error("Parameter must be a known constant")
      '{0}
```

We can easily know which extractors/constructors are needed using `Printer.TreeStructure.show`,
which returns the string representation the structure of the tree. Other printers
can also be found in the `Printer` module.

```scala
tree.show(using Printer.TreeStructure)
// or
Printer.TreeStructure.show(tree)
```

Bear in mind that extractors and constructors for the same trees might be comprised of different arguments, e.g. for `ValDef` the `apply` method
has `(Symbol, Option[Term])` arguments and `unapply` has `(String, TypeTree, Option[Term])` (if we want to obtain the symbol directly, we can call `.symbol` on the `ValDef`).

### Symbols
To construct definition `Trees` we might have to create or use a `Symbol`. Symbols represent the "named" parts of the code, the declarations we can reference elsewhere later. Let’s try to create `val name: Int = 0` from scratch.
To create a val like this, we need to first create a `Symbol` that matches the intended `Tree` type, so for a `ValDef` we would use the `Symbol.newVal` method:
```scala
  import quotes.reflect._
  val fooSym = Symbol.newVal(
    parent = Symbol.spliceOwner,
    name = "foo",
    tpe = TypeRepr.of[Int],
    flags = Flags.EmptyFlags,
    privateWithin = Symbol.noSymbol
  )
  val tree = ValDef(fooSym, Some(Literal(IntConstant(0))))
```
Generally, every `Symbol` needs to have an parent/owner `Symbol`, signifying where it is defined.
E.g if we want to define the val as part of a class, then naturally, we need that class' symbol to be the owner of the val symbol.
You may also notice the flags and privateWithin arguments, which are explained later in the `Flags` chapter.

The created val can be later referenced in other parts of the generated code with the use of `Ref` (a subtype of `Term`):
```scala
  Ref(fooSym)
```
For referencing types (e.g. ones created with `Symbol.newType` or `Symbol.newClass`), use `TypeIdent` (a subtype of `TypeTree`) instead.

#### Flags
`Flags` tell us about various attributes of `Symbols`. These can include access modifiers,
whether the symbol was defined in Scala 2 or Java, whether it's `inline` or `transparent`, whether it was generated by the compiler, etc.

They are implemented as a bit set, with the `.is` method allowing to check if a given `Flags` is a subset, and `.|` with `.&` allowing to
get a union or intersection respectively. You can see the available individual `Flags` from which to create the sets in the
[api documentation](https://scala-lang.org/api/3.3_LTS/scala/quoted/Quotes$reflectModule$FlagsModule.html).

It's worth thinking about individual `Flags` more in terms of explicitly stated modifiers, instead of general attributes.
For example, while we might say that every trait is `abstract`, a symbol of a trait will not have their `abstract` flag set
(just the `trait` flag instead), simply because it does not make sense to have an `abstract trait`.

Different types of Symbols have different flags allowed to be set, as stated in the API docs for individual `Symbol` constructor methods.

### TypeReprs and TypeTrees
When writing macros, we have access to `scala.quoted.Type`, which we can use to assign types in quoted code.
In the context of the reflection api however, it won't be of much use. We can convert it into a more useful
`TypeRepr` with `TypeRepr.of[T]` (when we have a given Type[T] in scope) which we can also convert back into a `Type`, with the simplest method being:
```scala
typeRepr.asType match
  case '[t] =>
    // access to a given Type[t] in scope
```

`TypeRepr`s are a type representation used when assigning and reading types from `Symbols`. It can be constructed/read similarly to the Typed AST trees. E.g.:
```Scala
  List[String]
```
is represented as:
```scala
  AppliedType(
    TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class collection)),object immutable),List),
    List(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class java)),object lang),String))
  )
```
Similarly to [Typed ASTs](#typed-abstract-syntax-trees), you can find the `TypeRepr` type hierarchy in
[reflectModule](https://scala-lang.org/api/3.3_LTS/scala/quoted/Quotes$reflectModule.html) docs.
Most of the nodes like `AppliedType` `AndType`, `MethodType`, etc. should be self explanatory,
but `TypeRef` and `TermRef` might require some additional context:
* `TypeRef(prefix, typeSymbol)` - corresponds to a selection of a type. E.g.: if `SomeType` is a type located in `prefix`,
and `someTypeSymbol` is its `Symbol`, `TypeRef(prefix, someTypeSymbol)` will correspond to prefix.SomeType
* `TermRef(prefix, termSymbol)` - corresponds to a selection on a term, which can also be useful if we are trying †o get a path dependent type.
E.g.: if `someVal` is a val in `prefix`, and `someValSymbol` is its symbol, then `TermRef(prefix, someValSymbol)` will correspond
to `prefix.someVal.type`. TermRef can be widened into their underlying non-TermRef type with `.widenByTermRef`.

Generally, if we need to insert a type directly as part of a tree (e.g. when passing it as a type parameter with a `TypeApply`),
we would use a `TypeTree` (subtype of `Tree`) instead.

#### Extracting TypeReprs from Symbols

Since `TypeReprs` allow us to create and analyse `Symbols`, we might expect there to be a method to obtain the type of a `Symbol`.
While there do exist `.typeRef` and `.termRef` methods, they can only generate TypeRefs or TermRefs that are usable only in
the scope of it's owner. E.g. for:
```scala
  val value: List[String] = List("")
```
If we were to call `.typeRef` on the symbol of value, we would get `TypeRef(This(...), valueSymbol)`, instead of `List[String]`.
This is because **Symbols hold incomplete type information**.
Let's look at the following:
```scala
class Outer[T]:
  val inner: List[T] = ???
```
The type of `inner` depends on the type parameter of `Outer` - so just having the symbol of `inner`
(which has no information about its prefix, in fact the symbols of `new Outer[Int].inner` and `new Outer[String].inner` are equal) is not enough.
However, we can still read the type if we have the prefixing `TypeRepr` with `prefix.memberType(symbol)` or `prefix.select(symbol)`:
```scala
val prefix = TypeRepr.of[Outer[String]]
val innerSymbol = Symbol.classMember
prefix.memberType(innerSymbol)
// The above returns:
//
// AppliedType(
//   TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class collection)),object immutable),List),
//   List(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class java)),object lang),String))
// )
```

### Navigating the API documentation
All Quotes reflection API documentation can be found inside of the
[reflectModule](https://scala-lang.org/api/3.3_LTS/scala/quoted/Quotes$reflectModule.html) trait in the scala library API docs.
Due to the implementation details, methods relevant to a certain type are split between `_Module` and `_Methods` traits.
For example, if we were to work on a `Select` node, the static methods like `apply` and `unapply` would be found in `SelectModule`,
and methods on instances of `Select` would be found in `SelectMethods`.

### Positions

The `Position` in the `quotes.reflect.*` provides an `ofMacroExpansion` value. It corresponds
to the expansion site for macros. The macro authors can obtain various information
about that expansion site. The example below shows how we can obtain position
information such as the start line, the end line or even the source code at the
expansion point.

```scala
def macroImpl()(quotes: Quotes): Expr[Unit] =
  import quotes.reflect.*
  val pos = Position.ofMacroExpansion

  val jpath = pos.sourceFile.getJPath.getOrElse(report.errorAndAbort("virtual file not supported", pos))
  val path = pos.sourceFile.path // fallback for a virtual file
  val start = pos.start
  val end = pos.end
  val startLine = pos.startLine
  val endLine = pos.endLine
  val startColumn = pos.startColumn
  val endColumn = pos.endColumn
  val sourceCode = pos.sourceCode
  ...
```

## Tree Utilities

`quotes.reflect` contains three facilities for tree traversal and
transformation.

`TreeAccumulator[X]` allows you to traverse the tree and aggregate data of type `X` along the way, by overriding its method `foldTree(x: X, tree: Tree)(owner: Symbol): X`.

`foldOverTree(x: X, tree: Tree)(owner: Symbol): X` calls `foldTree` on each children of `tree` (using `fold` to give each call the value of the previous one).

The code below, for example, collects the `val` definitions in the tree.

```scala
def collectPatternVariables(tree: Tree)(using ctx: Context): List[Symbol] =
  val acc = new TreeAccumulator[List[Symbol]]:
    def foldTree(syms: List[Symbol], tree: Tree)(owner: Symbol): List[Symbol] = tree match
      case ValDef(_, _, rhs) =>
        val newSyms = tree.symbol :: syms
        foldTree(newSyms, body)(tree.symbol)
      case _ =>
        foldOverTree(syms, tree)(owner)
  acc(Nil, tree)
```

A `TreeTraverser` extends a `TreeAccumulator[Unit]` and performs the same traversal
but without returning any value.

`TreeMap` transforms trees along the traversal, through overloading its methods it is possible to transform only trees of specific types, for example `transformStatement` only transforms `Statement`s.


### ValDef.let

The object `quotes.reflect.ValDef` also offers a method `let` that allows us to bind the `rhs` (right-hand side) to a `val` and use it in `body`.
Additionally, `lets` binds the given `terms` to names and allows to use them in the `body`.
Their type definitions are shown below:

```scala
def let(rhs: Term)(body: Ident => Term): Term = ...

def lets(terms: List[Term])(body: List[Term] => Term): Term = ...
```
