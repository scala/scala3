---
layout: doc-page
title: "Macros"
---

### Macros: Quotes and Splices

Macros are built on two well-known fundamental operations: quotation and
splicing.  Quotation is expressed as `'{...}` for expressions (both forms are
equivalent) and as `'[...]` for types. Splicing is expressed as `${ ... }`.
Additionally, within a quote or a splice we can quote or splice identifiers
directly (i.e. `'e` and `$e`). Readers may notice the resemblance of the two
aforementioned syntactic schemes with the familiar string interpolation syntax.

```scala
println(s"Hello, $name, here is the result of 1 + 1 = ${1 + 1}")
```

In string interpolation we _quoted_ a string and then we _spliced_ into it, two
others. The first, `name`, is a reference to a value of type `string`, and the
second is an arithmetic expression that will be _evaluated_ followed by the
splicing of its string representation.

Quotes and splices in this section allow us to treat code in a similar way,
effectively supporting macros. The entry point for macros is an inline method
with a top-level splice. We call it a top-level because it is the only occation
where we encounter a splice outside a quote (consider as a quote the
compilation-unit at the call-site). For example, the code below presents an
`inline` method `assert` which calls at compile-time a method `assertImpl` with
a boolean expression tree as argument. `assertImpl` evaluates the expression and
prints it again in an error message if it evaluates to `false`.

```scala
    import scala.quoted._

    inline def assert(expr: => Boolean): Unit =
      ${ assertImpl('expr) }

    def assertImpl(expr: Expr[Boolean]) = '{
      if !($expr) then
        throw new AssertionError(s"failed assertion: ${${ showExpr(expr) }}")
    }

    def showExpr(expr: Expr[Boolean]): Expr[String] =
      '{ "<some source code>" } // Better implementation later in this document
```

If `e` is an expression, then `'{e}` represents the typed
abstract syntax tree representing `e`. If `T` is a type, then `'[T]`
represents the type structure representing `T`.  The precise
definitions of "typed abstract syntax tree" or "type-structure" do not
matter for now, the terms are used only to give some
intuition. Conversely, `${e}` evaluates the expression `e`, which must
yield a typed abstract syntax tree or type structure, and embeds the
result as an expression (respectively, type) in the enclosing program.

Quotations can have spliced parts in them; in this case the embedded
splices are evaluated and embedded as part of the formation of the
quotation.

Quotes and splices can also be applied directly to identifiers. An identifier
`$x` starting with a `$` that appears inside a quoted expression or type is treated as a
splice `${x}`. Analogously, an quoted identifier 'x that appears inside a splice
is treated as a quote `'{x}`. See the Syntax section below for details.

Quotes and splices are duals of each other. For arbitrary
expressions `e` and types `T` we have:

    ${'{e}} = e
    '{${e}} = e
    ${'[T]} = T
    '[${T}] = T

### Types for Quotations

The type signatures of quotes and splices can be described using
two fundamental types:

  - `Expr[T]`: abstract syntax trees representing expressions of type `T`
  - `Type[T]`: type structures representing type `T`.

Quoting takes expressions of type `T` to expressions of type `Expr[T]`
and it takes types `T` to expressions of type `Type[T]`. Splicing
takes expressions of type `Expr[T]` to expressions of type `T` and it
takes expressions of type `Type[T]` to types `T`.

The two types can be defined in package `scala.quoted` as follows:
```scala
    package scala.quoted

    sealed abstract class Expr[+T]
    sealed abstract class Type[T]
```
Both `Expr` and `Type` are abstract and sealed, so all constructors for
these types are provided by the system. One way to construct values of
these types is by quoting, the other is by type-specific lifting
operations that will be discussed later on.

### The Phase Consistency Principle

A fundamental *phase consistency principle* (PCP) regulates accesses
to free variables in quoted and spliced code:

 - _For any free variable reference `x`, the number of quoted scopes and the number of spliced scopes between the reference to `x` and the definition of `x` must be equal_.

Here, `this`-references count as free variables. On the other
hand, we assume that all imports are fully expanded and that `_root_` is
not a free variable. So references to global definitions are
allowed everywhere.

The phase consistency principle can be motivated as follows: First,
suppose the result of a program `P` is some quoted text `'{ ... x
... }` that refers to a free variable `x` in `P` This can be
represented only by referring to the original variable `x`. Hence, the
result of the program will need to persist the program state itself as
one of its parts. We don’t want to do this, hence this situation
should be made illegal. Dually, suppose a top-level part of a program
is a spliced text `${ ... x ... }` that refers to a free variable `x`
in `P`.  This would mean that we refer during _construction_ of `P` to
a value that is available only during _execution_ of `P`. This is of
course impossible and therefore needs to be ruled out.  Now, the
small-step evaluation of a program will reduce quotes and splices in
equal measure using the cancellation rules above. But it will neither
create nor remove quotes or splices individually. So the PCP ensures
that program elaboration will lead to neither of the two unwanted
situations described above.

In what concerns the range of features it covers, this form of macros introduces
a principled meta programming framework that is quite close to the MetaML family of
languages. One difference is that MetaML does not have an equivalent of the PCP
- quoted code in MetaML _can_ access variables in its immediately enclosing
environment, with some restrictions and caveats since such accesses involve
serialization. However, this does not constitute a fundamental gain in
expressiveness.

### From `Expr`s to Functions and Back

The `Expr` companion object contains an implicit `AsFunction` conversion that turns a tree
describing a function into a function mapping trees to trees.
```scala
    object Expr {
      ...
      implicit class AsFunction[...](...) { ... }
    }
```
This decorator gives `Expr` the `apply` operation of an applicative functor, where `Expr`s
over function types can be applied to `Expr` arguments. The definition
of `AsFunction(f).apply(x)` is assumed to be functionally the same as
`'{($f)($x)}`, however it should optimize this call by returning the
result of beta-reducing `f(x)` if `f` is a known lambda expression.

The `AsFunction` decorator distributes applications of `Expr` over function
arrows:
```scala
    AsFunction(_).apply: Expr[S => T] => (Expr[S] => Expr[T])
```
Its dual, let’s call it `reflect`, can be defined as follows:
```scala
    def reflect[T, U](f: Expr[T] => Expr[U]): Expr[T => U] = '{
      (x: T) => ${ f('x) }
    }
```
Note how the fundamental phase consistency principle works in two
different directions here for `f` and `x`.  The reference to `f` is
legal because it is quoted, then spliced, whereas the reference to `x`
is legal because it is spliced, then quoted.

### Types and the PCP

In principle, The phase consistency principle applies to types as well
as for expressions. This might seem too restrictive. Indeed, the
definition of `reflect` above is not phase correct since there is a
quote but no splice between the parameter binding of `T` and its
usage. But the code can be made phase correct by adding a binding
of a `Type[T]` tag:
```scala
    def reflect[T, U](f: Expr[T] => Expr[U]) given (t: Type[T]): Expr[T => U] =
      '{ (x: $t) => ${ f('x) } }
```
In this version of `reflect`, the type of `x` is now the result of
splicing the `Type` value `t`. This operation _is_ splice correct -- there
is one quote and one splice between the use of `t` and its definition.

To avoid clutter, the Scala implementation tries to convert any phase-incorrect
reference to a type `T` to a type-splice, by rewriting `T` to `${ the[Type[T]] }`.
For instance, the user-level definition of `reflect`:

```scala
    def reflect[T: Type, U: Type](f: Expr[T] => Expr[U]): Expr[T => U] =
      '{ (x: T) => ${ f('x) } }
```
would be rewritten to
```scala
    def reflect[T: Type, U: Type](f: Expr[T] => Expr[U]): Expr[T => U] =
      '{ (x: ${ the[Type[T]] }) => ${ f('x) } }
```
The `the` query succeeds because there is a delegate of
type `Type[T]` available (namely the given parameter corresponding
to the context bound `: Type`), and the reference to that value is
phase-correct. If that was not the case, the phase inconsistency for
`T` would be reported as an error.

### Lifting Expressions

Consider the following implementation of a staged interpreter that implements
a compiler through staging.
```scala
    import scala.quoted._

    enum Exp {
      case Num(n: Int)
      case Plus(e1: Exp, e2: Exp)
      case Var(x: String)
      case Let(x: String, e: Exp, in: Exp)
    }
```
The interpreted language consists of numbers `Num`, addition `Plus`, and variables
`Var` which are bound by `Let`. Here are two sample expressions in the language:
```scala
    val exp = Plus(Plus(Num(2), Var("x")), Num(4))
    val letExp = Let("x", Num(3), exp)
```
Here’s a compiler that maps an expression given in the interpreted
language to quoted Scala code of type `Expr[Int]`.
The compiler takes an environment that maps variable names to Scala `Expr`s.
```scala
    import delegate scala.quoted._

    def compile(e: Exp, env: Map[String, Expr[Int]]): Expr[Int] = e match {
      case Num(n) =>
        n.toExpr
      case Plus(e1, e2) =>
        '{ ${ compile(e1, env) } + ${ compile(e2, env) } }
      case Var(x) =>
        env(x)
      case Let(x, e, body) =>
        '{ val y = ${ compile(e, env) }; ${ compile(body, env + (x -> 'y)) } }
    }
```
Running `compile(letExp, Map())` would yield the following Scala code:
```scala
    '{ val y = 3; (2 + y) + 4 }
```
The body of the first clause, `case Num(n) => n.toExpr`, looks suspicious. `n`
is declared as an `Int`, yet it is converted to an `Expr[Int]` with `toExpr`.
Shouldn’t `n` be quoted? In fact this would not
work since replacing `n` by `'n` in the clause would not be phase
correct.

The `toExpr` extension method is defined in package `quoted`:
```scala
    package quoted

    delegate LiftingOps {
      def (x: T) toExpr[T] given (ev: Liftable[T]): Expr[T] = ev.toExpr(x)
    }
```
The extension says that values of types implementing the `Liftable` type class can be
converted ("lifted") to `Expr` values using `toExpr`, provided a delegate import of `scala.quoted._` is in scope.

Dotty comes with delegate definitions of `Liftable` for
several types including `Boolean`, `String`, and all primitive number
types. For example, `Int` values can be converted to `Expr[Int]`
values by wrapping the value in a `Literal` tree node. This makes use
of the underlying tree representation in the compiler for
efficiency. But the `Liftable` instances are nevertheless not _magic_
in the sense that they could all be defined in a user program without
knowing anything about the representation of `Expr` trees. For
instance, here is a possible instance of `Liftable[Boolean]`:
```scala
    delegate for Liftable[Boolean] {
      def toExpr(b: Boolean) = if (b) '{ true } else '{ false }
    }
```
Once we can lift bits, we can work our way up. For instance, here is a
possible implementation of `Liftable[Int]` that does not use the underlying
tree machinery:
```scala
    delegate for Liftable[Int] {
      def toExpr(n: Int): Expr[Int] = n match {
        case Int.MinValue    => '{ Int.MinValue }
        case _ if n < 0      => '{ - ${ toExpr(-n) } }
        case 0               => '{ 0 }
        case _ if n % 2 == 0 => '{ ${ toExpr(n / 2) } * 2 }
        case _               => '{ ${ toExpr(n / 2) } * 2 + 1 }
      }
    }
```
Since `Liftable` is a type class, its instances can be conditional. For example,
a `List` is liftable if its element type is:
```scala
    delegate [T: Liftable] for Liftable[List[T]] {
      def toExpr(xs: List[T]): Expr[List[T]] = xs match {
        case head :: tail => '{ ${ toExpr(head) } :: ${ toExpr(tail) } }
        case Nil => '{ Nil: List[T] }
      }
    }
```
In the end, `Liftable` resembles very much a serialization
framework. Like the latter it can be derived systematically for all
collections, case classes and enums. Note also that the synthesis
of _type-tag_ values of type `Type[T]` is essentially the type-level
analogue of lifting.

Using lifting, we can now give the missing definition of `showExpr` in the introductory example:
```scala
    def showExpr[T](expr: Expr[T]): Expr[String] = {
      val code: String = expr.show
      code.toExpr
    }
```
That is, the `showExpr` method converts its `Expr` argument to a string (`code`), and lifts
the result back to an `Expr[String]` using the `toExpr` method.

**Note**: the `toExpr` extension method can be ommited by importing an implicit
conversion with `import scala.quoted.autolift._`. The programmer is able to
declutter slightly the code at the cost of readable _phase distinction_ between
stages.

### Lifting Types

The previous section has shown that the metaprogramming framework has
to be able to take a type `T` and convert it to a type tree of type
`Type[T]` that can be reified. This means that all free variables of
the type tree refer to types and values defined in the current stage.

For a reference to a global class, this is easy: Just issue the fully
qualified name of the class. Members of reifiable types are handled by
just reifying the containing type together with the member name. But
what to do for references to type parameters or local type definitions
that are not defined in the current stage? Here, we cannot construct
the `Type[T]` tree directly, so we need to get it from a recursive
implicit search. For instance, to implement
```scala
    the[Type[List[T]]]
```
where `T` is not defined in the current stage, we construct the type constructor
of `List` applied to the splice of the result of searching for a delegate for `Type[T]`:
```scala
    '[ List[ ${ the[Type[T]] } ] ]
```
This is exactly the algorithm that Scala 2 uses to search for type tags.
In fact Scala 2's type tag feature can be understood as a more ad-hoc version of
`quoted.Type`. As was the case for type tags, the implicit search for a `quoted.Type`
is handled by the compiler, using the algorithm sketched above.

### Relationship with Inline

Seen by itself, principled meta-programming looks more like a framework for
runtime metaprogramming than one for compile-time meta programming with macros.
But combined with Dotty’s `inline` feature it can be turned into a compile-time
system. The idea is that macro elaboration can be understood as a combination of
a macro library and a quoted program. For instance, here’s the `assert` macro
again together with a program that calls `assert`.

```scala
    object Macros {

      inline def assert(expr: => Boolean): Unit =
        ${ assertImpl('expr) }

      def assertImpl(expr: Expr[Boolean]) =
        '{ if !($expr) then throw new AssertionError(s"failed assertion: ${$expr}") }
    }

    object App {
      val program = {
        val x = 1
        Macros.assert(x != 0)
      }
    }
```
Inlining the `assert` function would give the following program:
```scala
    val program = {
      val x = 1
      ${ Macros.assertImpl('{ x != 0) } }
    }
```
The example is only phase correct because Macros is a global value and
as such not subject to phase consistency checking. Conceptually that’s
a bit unsatisfactory. If the PCP is so fundamental, it should be
applicable without the global value exception. But in the example as
given this does not hold since both `assert` and `program` call
`assertImpl` with a splice but no quote.

However, one could argue that the example is really missing
an important aspect: The macro library has to be compiled in a phase
prior to the program using it, but in the code above, macro
and program are defined together. A more accurate view of
macros would be to have the user program be in a phase after the macro
definitions, reflecting the fact that macros have to be defined and
compiled before they are used. Hence, conceptually the program part
should be treated by the compiler as if it was quoted:
```scala
    val program = '{
      val x = 1
      ${ Macros.assertImpl('{ x != 0 }) }
    }
```
If `program` is treated as a quoted expression, the call to
`Macro.assertImpl` becomes phase correct even if macro library and
program are conceptualized as local definitions.

But what about the call from `assert` to `assertImpl`? Here, we need a
tweak of the typing rules. An inline function such as `assert` that
contains a splice operation outside an enclosing quote is called a
_macro_. Macros are supposed to be expanded in a subsequent phase,
i.e. in a quoted context. Therefore, they are also type checked as if
they were in a quoted context. For instance, the definition of
`assert` is typechecked as if it appeared inside quotes.  This makes
the call from `assert` to `assertImpl` phase-correct, even if we
assume that both definitions are local.

The `inline` modifier is used to declare a `val` that is
either a constant or is a parameter that will be a constant when instantiated. This
aspect is also important for macro expansion.  To illustrate this,
consider an implementation of the `power` function that makes use of a
statically known exponent:
```scala
    inline def power(inline n: Int, x: Double) = ${ powerCode(n, 'x) }

    private def powerCode(n: Int, x: Expr[Double]): Expr[Double] =
      if (n == 0) '{ 1.0 }
      else if (n == 1) x
      else if (n % 2 == 0) '{ val y = $x * $x; ${ powerCode(n / 2, 'y) } }
      else '{ $x * ${ powerCode(n - 1, x) } }
```
The reference to `n` as an argument in `${ powerCode(n, 'x) }` is not
phase-consistent, since `n` appears in a splice without an enclosing
quote. Normally that would be a problem because it means that we need
the _value_ of `n` at compile time, which is not available for general
parameters. But since `n` is an inline parameter of a macro, we know
that at the macro’s expansion point `n` will be instantiated to a
constant, so the value of `n` will in fact be known at this
point. To reflect this, we loosen the phase consistency requirements
as follows:

 - If `x` is a inline value (or a inline parameter of an inline
   function) of type Boolean, Byte, Short, Int, Long, Float, Double,
   Char or String, it can be accessed in all contexts where the number
   of splices minus the number of quotes between use and definition
   is either 0 or 1.

### Scope Extrusion

Quotes and splices are duals as far as the PCP is concerned. But there is an
additional restriction that needs to be imposed on splices to guarantee
soundness: code in splices must be free of side effects. The restriction
prevents code like this:

```scala
  var x: Expr[T] = ...
  '{ (y: T) => ${ x = 'y; 1 } }
```

This code, if it was accepted, would _extrude_ a reference to a quoted variable
`y` from its scope. This would subsequently allow access to a variable outside the
scope where it is defined, which is likely problematic. The code is clearly
phase consistent, so we cannot use PCP to rule it out. Instead we postulate a
future effect system that can guarantee that splices are pure. In the absence of
such a system we simply demand that spliced expressions are pure by convention,
and allow for undefined compiler behavior if they are not. This is analogous to
the status of pattern guards in Scala, which are also required, but not
verified, to be pure.

[Multi-Stage Programming](./staging.html) introduces one additional methods where
you can expand code at runtime with a method `run`. There is also a problem with
that invokation of `run` in splices. Consider the following expression:

```scala
    '{ (x: Int) => ${ ('x).run; 1 } }
```
This is again phase correct, but will lead us into trouble. Indeed, evaluating
the splice will reduce the expression `('x).run` to `x`. But then the result

```scala
    '{ (x: Int) => ${ x; 1 } }
```

is no longer phase correct. To prevent this soundness hole it seems easiest to
classify `run` as a side-effecting operation. It would thus be prevented from
appearing in splices. In a base language with side-effects we'd have to do this
anyway: Since `run` runs arbitrary code it can always produce a side effect if
the code it runs produces one.

### Example Expansion

Assume we have two methods, one `map` that takes an `Expr[Array[T]]` and a
function `f` and one `sum` that performs a sum by delegating to `map`.

```scala
object Macros {
  def map[T](arr: Expr[Array[T]], f: Expr[T] => Expr[Unit])(implicit t: Type[T]): Expr[Unit] = '{
    var i: Int = 0
    while (i < ($arr).length) {
      val element: $t = ($arr)(i)
      ${f('element)}
      i += 1
    }
  }

  def sum(arr: Expr[Array[Int]]): Expr[Int] = '{
    var sum = 0
    ${ map(arr, x => '{sum += $x}) }
    sum
  }

  inline def sum_m(arr: Array[Int]): Int = ${sum('arr)}
}
```

A call to `sum_m(Array(1,2,3))` will first inline `sum_m`:

```scala
val arr: Array[Int] = Array.apply(1, [2,3 : Int]:Int*)
${_root_.Macros.sum('arr)}
```

then it will splice `sum`:

```scala
val arr: Array[Int] = Array.apply(1, [2,3 : Int]:Int*)

var sum = 0
${ map(arr, x => '{sum += $x}) }
sum
```

then it will inline `map`:

```scala
val arr: Array[Int] = Array.apply(1, [2,3 : Int]:Int*)

var sum = 0
val f = x => '{sum += $x}
${ _root_.Macros.map(arr,  '[Int], 'f)}
sum
```

then it will expand and splice inside quotes `map`:

```scala
val arr: Array[Int] = Array.apply(1, [2,3 : Int]:Int*)

var sum = 0
val f = x => '{sum += $x}
var i: Int = 0
while (i < (arr).length) {
  val element: Int = (arr)(i)
  sum += element
  i += 1
}
sum
```

Finally cleanups and dead code elimination:
```scala
val arr: Array[Int] = Array.apply(1, [2,3 : Int]:Int*)
var sum = 0
var i: Int = 0
while (i < arr.length) {
  val element: Int = arr(i)
  sum += element
  i += 1
}
sum
```

### Relationship with Whitebox Inline

[Inline](./inline.html) documents inlining. The code below introduces a whitebox
inline method that can calculate either a value of type `Int` or a value of type
`String`.

```scala
inline def defaultOf(inline str: String) <: Any = ${ defaultOfImpl(str) }

def defaultOfImpl(str: String): Expr[Any] = str match {
  case "int" => '{1}
  case "string" => '{"a"}
}

// in a separate file
val a: Int = defaultOf("int")
val b: String = defaultOf("string")
```

[More details](./macros-spec.html)
