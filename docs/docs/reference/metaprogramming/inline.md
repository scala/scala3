---
layout: doc-page
title: Inline
---

## Inline (blackbox/whitebox)

`inline` is a new [soft modifier](../soft-modifier.html) that guarantees that a
definition will be inlined at the point of use. Example:

```scala
object Config {
  inline val logging = false
}

object Logger {

  private var indent = 0

  inline def log[T](msg: => String)(op: => T): T =
    if (Config.logging) {
      println(s"${"  " * indent}start $msg")
      indent += 1
      val result = op
      indent -= 1
      println(s"${"  " * indent}$msg = $result")
      result
    }
    else op
}
```

The `Config` object contains a definition of the **inline value** `logging`.
This means that `logging` is treated as a _constant value_, equivalent to its
right-hand side `false`. The right-hand side of such an `inline val` must itself
be a [constant expression](https://scala-lang.org/files/archive/spec/2.12/06-expressions.html#constant-expressions). Used in this
way, `inline` is equivalent to Java and Scala 2's `final`. `final` meaning
_inlined constant_ is still supported in Dotty, but will be phased out.

The `Logger` object contains a definition of the **inline method** `log`.
This method will always be inlined at the point of call.

In the inlined code, an if-then-else with a constant condition will be rewritten
to its then- or else-part. Consequently, in the `log` method above
`if (Config.logging)` with `Config.logging == true` will rewritten into its then-part.

Here's an example:

```scala
def factorial(n: BigInt): BigInt =
  log(s"factorial($n)") {
    if (n == 0) 1
    else n * factorial(n - 1)
  }
```

If `Config.logging == false`, this will be rewritten (simplified) to

```scala
def factorial(n: BigInt): BigInt = {
  if (n == 0) 1
  else n * factorial(n - 1)
}
```

and if `true` it will be rewritten to the code below:

```scala
def factorial(n: BigInt): BigInt = {
  val msgVal = s"factorial($n)"
  println(s"${"  " * indent}start $msgVal")
  Logger.inline$indent += 1
  val result = op
  Logger.inline$indent -= 1
  println(s"${"  " * indent}$msgVal = $result")
  result
}
```
TODO: adapt to real code.
Note (1) that the arguments corresponding to the parameters `msg` and `op` of
the inline method `log` are defined before the inlined body (which is in this
case simply `op` (2)). By-name parameters of the inline method correspond to
`def` bindings whereas by-value parameters correspond to `val` bindings. So if
`log` was defined like this:

```scala
inline def log[T](msg: String)(op: => T): T = ...
```

we'd get

```scala
val msg = s"factorial($n)"
```

instead. This behavior is designed so that calling an inline method is
semantically the same as calling a normal method: By-value arguments are
evaluated before the call whereas by-name arguments are evaluated each time they
are referenced. As a consequence, it is often preferable to make arguments of
inline methods by-name in order to avoid unnecessary evaluations. Additionally,
in the code above, our goal is to print the result after the evaluation of `op`.
Imagine, if we were printing the duration of the evaluation between the two
prints.

For instance, here is how we can define a zero-overhead `foreach` method that
translates into a straightforward while loop without any indirection or
overhead:

```scala
inline def foreach(op: => Int => Unit): Unit = {
  var i = from
  while (i < end) {
    op(i)
    i += 1
  }
}
```

By contrast, if `op` is a call-by-value parameter, it would be evaluated
separately as a closure.

Inline methods can be recursive. For instance, when called with a constant
exponent `n`, the following method for `power` will be implemented by
straight inline code without any loop or recursion.

```scala
inline def power(x: Double, n: Int): Double = {
  if (n == 0) 1.0
  else if (n == 1) x
  else {
    val y = power(x, n / 2)
    if (n % 2 == 0) y * y else y * y * x
  }

  power(expr, 10)
    // translates to
    //
    //    val x = expr
    //    val y1 = x * x   // ^2
    //    val y2 = y1 * y1 // ^4
    //    val y3 = y2 * x  // ^5
    //    y3 * y3          // ^10
}
```

Parameters of inline methods can be marked `inline`. This means
that actual arguments to these parameters must be constant expressions.
For example:

```scala
inline def power(x: Double, inline n: Int): Double
```

### Relationship to @inline

Scala also defines a `@inline` annotation which is used as a hint
for the backend to inline. The `inline` modifier is a more powerful
option: Expansion is guaranteed instead of best effort,
it happens in the frontend instead of in the backend, and it also applies
to recursive methods.

To cross compile between both Dotty and Scalac, we introduce a new `@forceInline`
annotation which is equivalent to the new `inline` modifier. Note that
Scala 2 ignores the `@forceInline` annotation, so one must use both
annotations to guarantee inlining for Dotty and at the same time hint inlining
for Scala 2 (i.e. `@forceInline @inline`).

### Evaluation Rules

As you noticed by the examples above a lambda of the form

`((x_1, ..., x_n) => B)(E_1, ..., E_n)` is rewritten to:

```
{ val/def x_1 = E_1
  ...
  val/def x_n = E_n
  B
}
```

where vals are used for value parameter bindings and defs are used for by-name
parameter bindings. If an argument `E_i` is a simple variable reference `y`, the
corresponding binding is omitted and `y` is used instead of `x_i` in `B`.

If a `inline` modifier is given for parameters, corresponding arguments must be
pure expressions of constant type.

#### The definition of constant expression

Right-hand sides of inline values and of arguments for inline parameters must be
constant expressions in the sense defined by the [SLS §
6.24](https://www.scala-lang.org/files/archive/spec/2.12/06-expressions.html#constant-expressions),
including _platform-specific_ extensions such as constant folding of pure
numeric computations.

### Specializing Inline (Whitebox)

Inline methods support the ` <: T` return type syntax. This means that the return type
of the inline method is going to be specialized to a more precise type upon
expansion.

Consider the example below where the inline method `choose` can return an object
of any of the two dynamic types. The subtype relationship is `B <: A`. Since we
use the specializing inline syntax, the static types of the `val`s are inferred
accordingly. Consequently, calling `meth` on `obj2` is not a compile-time error
as `obj2` will be of type `B`.

```scala
class A
class B extends A {
  def meth() = true
}

inline def choose(b: Boolean) <: A = {
  if (b) new A()
  else new B()
}

val obj1 = choose(true)  // static type is A
val obj2 = choose(false) // static type is B

obj1.meth() // compile-time error
obj2.meth() // OK
```

In the following example, we see how the return type of `zero` is specialized to
the singleton type `0` permitting the addition to be ascribed with the correct
type `1`.

```scala
inline def zero() <: Int = 0

final val one: 1 = zero() + 1
```

#### Inline Match

A `match` expression in the body of an `inline` method definition may be
prefixed by the `inline` modifier. If there is enough static information to
unambiguously take a branch, the expression is reduced to that branch and the
type of the result is taken. The example below defines an inline method with a
single inline match expression that picks a case based on its static type:

```scala
inline def g(x: Any) <: Any = inline x match {
  case x: String => (x, x) // Tuple2[String, String](x, x)
  case x: Double => x
}

g(1.0d) // Has type 1.0d which is a subtype of Double
g("test") // Has type (String, String)
```

The scrutinee `x` is examined statically and the inline match is reduced
accordingly returning the corresponding value (with the type specialized due to
the `<:` in the return type). This example performs a simple type test over the
scrutinee. The type can have a richer structure like the simple ADT below.
`toInt` matches the structure of a number in Church-encoding and _computes_ the
corresponding integer.

```scala
trait Nat
case object Zero extends Nat
case class Succ[N <: Nat](n: N) extends Nat

inline def toInt(n: Nat) <: Int = inline n match {
  case Zero => 0
  case Succ(n1) => toInt(n1) + 1
}

final val natTwo = toInt(Succ(Succ(Zero)))
val intTwo: 2 = natTwo
```

`natTwo` is inferred to have the singleton type 2.

#### scala.compiletime._

This package contains helper definitions providing support for compile time
operations over values.

##### Const Value & Const Value Opt

`constvalue` is a function that produces the constant value represented by a
type.

```scala
import scala.compiletime.{constValue, S}

inline def toIntC[N] <: Int =
  inline constValue[N] match {
    case 0 => 0
    case _: S[n1] => 1 + toIntC[n1]
  }

final val ctwo = toIntC[2]
```

`constValueOpt` is the same as `constValue`, however returning an `Option[T]`
enabling us to handle situations where a value is not present. Note that `S` is
the type of the successor of some singleton type. For example the type `S[1]` is
the singleton type `2`.

##### Erased Value

We have seen so far inline methods that take terms (tuples and integers) as
parameters. What if we want to base case distinctions on types instead? For
instance, one would like to be able to write a function `defaultValue`, that,
given a type `T` returns optionally the default value of `T`, if it exists. In
fact, we can already express this using rewrite match expressions and a simple
helper function, `scala.compiletime.erasedValue`, which is defined as follows:

```scala
erased def erasedValue[T]: T = ???
```

The `erasedValue` function _pretends_ to return a value of its type argument
`T`. In fact, it would always raise a `NotImplementedError` exception when
called. But the function can in fact never be called, since it is declared
`erased`, so can be only used a compile-time during type checking.

Using `erasedValue`, we can then define `defaultValue` as follows:

```scala
inline def defaultValue[T] = inline erasedValue[T] match {
  case _: Byte => Some(0: Byte)
  case _: Char => Some(0: Char)
  case _: Short => Some(0: Short)
  case _: Int => Some(0)
  case _: Long => Some(0L)
  case _: Float => Some(0.0f)
  case _: Double => Some(0.0d)
  case _: Boolean => Some(false)
  case _: Unit => Some(())
  case _: t >: Null => Some(null)
  case _ => None
}
```

Then:
```scala
defaultValue[Int] = Some(0)
defaultValue[Boolean] = Some(false)
defaultValue[String | Null] = Some(null)
defaultValue[AnyVal] = None
```

As another example, consider the type-level version of `toNat` above: given a
_type_ representing a Peano number, return the integer _value_ corresponding to
it. Here's how this can be defined:

```scala
inline def toInt[N <: Nat] <: Int = inline scala.compiletime.erasedValue[N] match {
  case _: Zero => 0
  case _: Succ[n] => toIntT[n] + 1
}

final val two = toInt[Succ[Succ[Zero]]]
```

`erasedValue` is an `erased` method so it cannot be used and has no runtime
behavior. Since `toInt` performs static checks over the static type of `N` we
can safely use it to scrutinize its return type (`S[S[Z]]` in this case).

##### Error

This package provides a compile time `error` definition with the following signature:

```scala
inline def error(inline msg: String, objs: Any*): Nothing
```

The purpose of this is to expand at the point of use, an error message (a
constant string) and append with commas, compile time values passed in the
`objs` param.

#### Implicit Match

It is foreseen that many areas of typelevel programming can be done with rewrite
methods instead of implicits. But sometimes implicits are unavoidable. The
problem so far was that the Prolog-like programming style of implicit search
becomes viral: Once some construct depends on implicit search it has to be
written as a logic program itself. Consider for instance the problem of creating
a `TreeSet[T]` or a `HashSet[T]` depending on whether `T` has an `Ordering` or
not. We can create a set of implicit definitions like this:

```scala
trait SetFor[T, S <: Set[T]]
class LowPriority {
  implicit def hashSetFor[T]: SetFor[T, HashSet[T]] = ...
}
object SetsFor extends LowPriority {
  implicit def treeSetFor[T: Ordering]: SetFor[T, TreeSet[T]] = ...
}
```

Clearly, this is not pretty. Besides all the usual indirection of implicit
search, we face the problem of rule prioritization where we have to ensure that
`treeSetFor` takes priority over `hashSetFor` if the element type has an
ordering. This is solved (clumsily) by putting `hashSetFor` in a superclass
`LowPriority` of the object `SetsFor` where `treeSetFor` is defined. Maybe the
boilerplate would still be acceptable if the crufty code could be contained.
However, this is not the case. Every user of the abstraction has to be
parameterized itself with a `SetFor` implicit. Considering the simple task _"I
want a `TreeSet[T]` if `T` has an ordering and a `HashSet[T]` otherwise"_, this
seems like a lot of ceremony.

There are some proposals to improve the situation in specific areas, for
instance by allowing more elaborate schemes to specify priorities. But they all
keep the viral nature of implicit search programs based on logic programming.

By contrast, the new `implicit match` construct makes implicit search available
in a functional context. To solve the problem of creating the right set, one
would use it as follows:
```scala
inline def setFor[T]: Set[T] = implicit match {
  case ord: Ordering[T] => new TreeSet[T]
  case _                => new HashSet[T]
}
```
An implicit match uses the `implicit` keyword in the place of the scrutinee. Its
patterns are type ascriptions of the form `identifier : Type`.

Patterns are tried in sequence. The first case with a pattern `x: T` such that
an implicit value of type `T` can be summoned is chosen. The variable `x` is
then bound to the implicit value for the remainder of the case. It can in turn
be used as an implicit in the right hand side of the case. It is an error if one
of the tested patterns gives rise to an ambiguous implicit search.

An implicit matches is considered to be a special kind of a inline match. This
means it can only occur in the body of an inline method, and it must be reduced
at compile time.

Consequently, if we summon an `Ordering[String]` the code above will return a
new instance of `TreeSet[String]`.

```scala
the[Ordering[String]]

println(setFor[String].getClass) // prints class scala.collection.immutable.TreeSet
```

**Note** implicit matches can raise ambiguity errors. Consider the following
code with two implicit values in scope of type `A`. The single pattern match
case of the implicit match with type ascription of an `A` raises the ambiguity
error.

```scala
class A
implicit val a1: A = new A
implicit val a2: A = new A

inline def f: Any = implicit match {
  case _: A => ???  // error: ambiguous implicits
}
```

### Reference

For more info, see [PR #4927](https://github.com/lampepfl/dotty/pull/4768),
which explains how inline methods can be used for typelevel programming and code
specialization.