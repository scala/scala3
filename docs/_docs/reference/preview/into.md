---
layout: doc-page
title: The `into` Type and Modifier
redirectFrom: /experimental/into.html
nightlyOf: https://docs.scala-lang.org/scala3/reference/preview/into.html
---

This feature is available as a preview since Scala 3.8.0.


## Summary

Scala 3 offers two alternative schemes to allow implicit conversions using Scala-3's `Conversion`
class without requiring a language import.

The first scheme is
to have a special type `into[T]` which serves as a marker that conversions into that type are allowed. These types are typically used in parameters of methods that are designed to work with implicit conversions of their arguments. This allows fine-grained control over where implicit conversions should be allowed. We call this scheme "_into as a type constructor_".

The second scheme allows `into` as a soft modifier on traits, classes, and opaque type aliases. If a type definition is declared with this modifier, conversions to that type are allowed. The second scheme requires that one has control over the conversion target types so that an `into` can be added to their declaration. It is appropriate where there are a few designated types that are meant to be conversion targets. If that's the case, migration from Scala 2 to Scala 3
becomes easier since no function signatures need to be rewritten. We call this scheme "_into as a modifier_".


## Motivation

Scala 3's implicit conversions of the `scala.Conversion` class require a language import
```
import scala.language.implicitConversions
```
in any code that uses them as implicit conversions (code that calls conversions explicitly is not affected). If the import is missing, a feature warning is currently issued, and this will become an error in future versions of Scala 3. The motivation for this restriction is two-fold:

 - Code with hidden implicit conversions is hard to understand and might have correctness or performance issues that go undetected.
 - If we require explicit user opt-in for implicit conversions, we can significantly improve type inference by propagating expected type information more widely in those parts of the program where there is no opt-in.

There is one broad use case, however, where implicit conversions are very hard to replace. This is the case where an implicit conversion is used to adapt a method argument to its formal parameter type. An example from the standard library:
```scala
scala> val xs = List(0, 1)
scala> val ys = Array(2, 3)
scala> xs ++ ys
val res0: List[Int] = List(0, 1, 2, 3)
```
The input line `xs ++ ys` makes use of an implicit conversion from `Array[Int]` to `IterableOnce[Int]`. This conversion is defined in the standard library as an `implicit def`. Once the standard library is rewritten with Scala 3 conversions, this will require a language import at the use site, which is clearly unacceptable. It is possible to avoid the need for implicit conversions using method overloading or type classes, but this often leads to longer and more complicated code, and neither of these alternatives work for vararg parameters.

## First Scheme: `into` as a Type Constructor

This is where the `into` type constructor comes in. Here is a signature of a `++` method on `List[A]` that uses it:

```scala
  def ++ (elems: into[IterableOnce[A]]): List[A]
```
The `into` wrapper on the type of `elems` means that implicit conversions can be applied to convert the actual argument to an `IterableOnce` value, and this without needing a language import.

`into` is defined as follows in the companion object of the `scala.Conversion` class:
```scala
opaque type into[T] >: T = T
```
Types of the form `into[T]` are treated specially during type checking. If the expected type of an expression is `into[T]` then an implicit conversion to that type can be inserted without the need for a language import.

Note: Unlike other types, `into` starts with a lower-case letter. This emphasizes the fact that `into` is treated specially by the compiler, by making `into` look more like a keyword than a regular type.

### Example 1

```scala
given Conversion[Array[Int], IterableOnce[Int]] = wrapIntArray
val xs: List[Int] = List(1)
val ys: Array[Int] = Array(2, 3)
xs ++ ys
```
This inserts the given conversion on the `ys` argument in `xs ++ ys`. It typechecks without a feature warning since the formal parameter of `++` is of type `into[IterableOnce]`, which is also the expected type of `ys`.

### Example 2

Consider a simple expression AST type:
```scala
enum Expr:
  case Neg(e: Expr)
  case Add(e1: Expr, e2: Expr)
  case Const(n: Int)
import Expr.*
```
Say we'd like to build `Expr` trees without explicit `Const` wrapping, as in `Add(1, Neg(2))`. The usual way to achieve this is with an implicit conversion from `Int` to `Const`:
```scala
given Conversion[Int, Const] = Const(_)
```
Normally, that would require a language import in all source modules that construct `Expr` trees. We can avoid this requirement on user code by declaring `Neg` and `Add` with `into` parameters:
```scala
enum Expr:
  case Neg(e: into[Expr])
  case Add(e1: into[Expr], e2: into[Expr])
  case Const(n: Int)
```
This would allow conversions from `Int` to `Const` when constructing trees but not elsewhere.

### `into` in Function Results

`into` allows conversions everywhere it appears as expected type, including in the results of function arguments. For instance, consider the new proposed signature of the `flatMap` method on `List[A]`:

```scala
  def flatMap[B](f: A => into[IterableOnce[B]]): List[B]
```
This accepts all actual arguments `f` that, when applied to an `A`, give a result
that is convertible to `IterableOnce[B]`. So the following would work:
```scala
scala> val xs = List(1, 2, 3)
scala> xs.flatMap(x => x.toString * x)
val res2: List[Char] = List(1, 2, 2, 3, 3, 3)
```
Here, the conversion from `String` to `Iterable[Char]` is applied on the results of `flatMap`'s function argument when it is applied to the elements of `xs`.

### Vararg arguments

When applied to a vararg parameter, `into` allows a conversion on each argument value individually. For example, consider a method `concatAll` that concatenates a variable
number of `IterableOnce[Char]` arguments, and also allows implicit conversions into `IterableOnce[Char]`:

```scala
def concatAll(xss: into[IterableOnce[Char]]*): List[Char] =
  xss.foldRight(Nil)(_ ++: _)
```
Here, the call
```scala
concatAll(List('a'), "bc", Array('d', 'e'))
```
would apply two _different_ implicit conversions: the conversion from `String` to `Iterable[Char]` gets applied to the second argument and the conversion from `Array[Char]` to `Iterable[Char]` gets applied to the third argument.


### Unwrapping `into`

Since `into[T]` is an opaque type, its run-time representation is just `T`.
At compile time, the type `into[T]` is a known supertype of the type `T`. So if `t: T`, then
```scala
  val x: into[T] = t
```
typechecks but
```scala
val y: T = x   // error
```
is ill-typed. We can recover the underlying type `T` using the `underlying` extension method which is also defined in object `Conversion`:
```scala
import Conversion.underlying

val y: T = x.underlying    // ok
```
However, the next section shows that unwrapping with `.underlying` is not needed for parameters, which is the most common use case. So explicit unwrapping should be quite rare.



### Dropping `into` for Parameters in Method Bodies

The typical use cases for `into` wrappers are for parameters. Here, they specify that the
corresponding arguments can be converted to the formal parameter types. On the other hand, inside a method, a parameter type can be assumed to be of the underlying type since the conversion already took place when the enclosing method was called. This is reflected in the type system which erases `into` wrappers in the local types of parameters
as they are seen in a method body. Here is an example:
```scala
  def ++ (elems: into[IterableOnce[A]]): List[A] =
    val buf = ListBuffer[A]()
    for elem <- elems.iterator do // no `.underlying` needed here
      buf += elems
    buf.toList
```
Inside the `++` method, the `elems` parameter is of type `IterableOnce[A]`, not `into[IterableOne[A]]`. Hence, we can simply write `elems.iterator` to get at the `iterator` method of the `IterableOnce` class.

Specifically, we erase all `into` wrappers in the local types of parameter types that appear in covariant or invariant position. Contravariant `into` wrappers are kept since these typically are on the parameters of function arguments.

### Into Constructors in Type Aliases

Since `into` is a regular type constructor, it can be used anywhere, including in type aliases and type parameters. For instance, in the Scala standard library we could define
```scala
type ToIterator[T] = into[IterableOnce[T]]
```
and then `++`, `flatMap` and other functions could use this alias in their parameter types. The effect would be the same as when `into` is written out explicitly.

## Second Scheme: `into` as a Modifier

The `into` scheme discussed so far strikes a nice balance between explicitness and convenience. But migrating to it from Scala 2 implicits does require major changes since possibly a large number of function signatures has to be changed to allow conversions on the arguments. This might ultimately hold back migration to Scala 3 implicits.

To facilitate migration, we also introduce an alternative way to specify target types of implicit conversions. We allow `into` as a soft modifier on
classes, traits, and opaque type aliases. If a type definition is declared with `into`, then implicit conversions into that type don't need a language import.

For instance, the Laminar framework
defines a trait `Modifier` that is commonly used as a parameter type of user-defined methods and that should support implicit conversions into it.
`Modifier` is commonly used as a parameter type in both Laminar framework functions and in application-level functions that use Laminar.

We can support implicit conversions to `Modifier`s simply by making `Modifier` an `into` trait:
```scala
into trait Modifier ...
```
This means implicit `Conversion` instances with `Modifier` results can be inserted without requiring a language import.

Here is a simplified example:
```scala
trait Modifier
given Conversion[Option[Node], Modifier] = ...
given Conversion[Seq[Node], Modifier] = ...

def f(x: Source, m: Modifier) = ...
f(source, Some(node)) // inserts conversion
```

The `into`-as-a-modifier scheme is handy in codebases that have a small set of specific types that are intended as the targets of implicit conversions defined in the same codebase. Laminar's `Modifier` is a typical example. But the scheme can be easily abused by making the number of `into` types too large. One should restrict the number of `into`-declared types to the absolute minimum. In particular, never make a type `into` to just cater for the possibility that someone might want to later add an implicit conversion to it.


## Details: Conversion target types

To make the preceding descriptions more precise: An implicit conversion is permitted without an `implicitConversions` language import if the target type is a valid conversion target type. A valid conversion target type is one of the following:

 - A type of the form `into[T]`.
 - A reference `p.C` to a class, trait, or opaque type alias `C` that is declared with an `into` modifier. The reference can be followed by type arguments.
 - A type alias of a valid conversion target type.
 - A match type that reduces to a valid conversion target type.
 - An annotated type `T @ann` where `T` is a valid conversion target type.
 - A refined type `T {...}` where `T` is a valid conversion target type.
 - A union `T | U` of two valid conversion target types `T` and `U`.
 - An intersection `T & U` of two valid conversion target types `T` and `U`.
 - An instance of a type parameter that is explicitly instantiated to a valid conversion target type.

Type parameters that are not fully instantiated do not count as valid conversion target types. For instance, consider:

```scala
  trait Token
  class Keyword(str: String)
  given Conversion[String, Keyword] = KeyWord(_)

  List[into[Keyword]]("if", "then", "else")
```
This type-checks since the target type of the list elements is the type parameter of the `List.apply` method which is explicitly instantiated to `into[Keyword]`. On the other hand, if we continue the example as follows we get an error:
```scala
  val ifKW: into[Keyword] = "if"
  val ys: List[into[Keyword]] = List(ifKW, "then", "else")
```
Here, the type variable of `List.apply` is not explicitly instantiated
when we check the `List(...)` arguments (it is just upper-bounded by the target type `into[Keyword]`). This is not enough to allow
implicit conversions on the second and third arguments.

Subclasses of `into` classes or traits do not count as valid conversion target types. For instance, consider:

```scala
into trait T
class C(x: Int) extends T
given Conversion[Int, C] = C(_)

def f(x: T) = ()
def g(x: C) = ()
f(1)      // ok
g(1)      // error
```
The call `f("abc")` type-checks since `f`'s parameter type `T` is `into`.
But the call `g("abc")` does not type-check since `g`'s parameter type `C` is not `into`. It does not matter that `C` extends a trait `T` that is `into`.


## Why Two Different Schemes?

Can we make do with just one scheme instead of two? In practice this would be difficult.

Let's first take a look the `Expr` example, which uses into-as-a-constructor. Could it be rewritten to use into-as-a-modifier?
This would mean we have to add `into` to the whole `Expr` enum. Adding it to just `Const` is not enough, since `Add` and `Neg` take `Expr` arguments, not `Const` arguments.

But we might not always have permission to change the `Expr` enum. For instance, `Expr` could be defined in a lower level library without implicit conversions, but later we want to make `Expr` construction convenient by eliding `Const` wrappers in some higher-level library or application. With `into` constructors, this is easy: Define the implicit conversion and facade methods that construct `Expr` trees while taking `into[Expr]` parameters.
With `into` modifiers there is no way to achieve the same.

A possibly more important objection is that even if we could add the `into` modifier to `Expr`, it would be bad style to do so! We want to allow for implicit conversion in the very specific case where we build an `Expr` tree using the `Add` and `Neg` constructors. Our applications could have lots of other methods that take `Expr` trees, for instance to analyze them or evaluate them.
We probably do not want to allow implicit conversions for the arguments of all these other methods. The `into` modifier is too unspecific to distinguish the good use case from the problematic ones.

On the other hand, there are also situations where into-as-a-modifier is the practical choice. To see this, consider again the `Modifier` use case in Laminar.
We could avoid the `into` modifier by wrapping all `Modifier` parameters
with the `into` constructor. This would be a lot more work than adding just the single `into` modifier. Worse, functions taking `Modifier` parameters are found both in the Laminar framework code and in many applications using it. The framework and the applications would have to be upgraded in lockstep. When Laminar upgrades to Scala 3 implicits, all applications would have to be rewritten, which would make such a migration very cumbersome.

One can try to mitigate the effort by playing with type aliases. For instance, a hypothetical future Laminar using Scala 3 conversions could rename the
trait `Modifier` to `ModifierTrait` and define an alias
```scala
type Modifier = into[ModifierTrait]
```
Then the source code of applications would not have to change (unless these applications define classes directly extending `Modifier`). But that future Laminar would not be binary compatible with the current one, since the name
of the original `Modifier` trait has changed. In summary, upgrading Laminar to use Scala 3 conversions could keep either source compatibility or binary compatibility but not both at the same time.


## Syntax Changes

```
LocalModifier     ::=  ...  |  ‘into’
```

`into` is a soft modifier. It is only allowed classes, traits, and opaque type aliases.

