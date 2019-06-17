---
layout: doc-page
title: Inline
---

## Inline Definitions

`inline` is a new [soft modifier](../soft-modifier.html) that guarantees that a
definition will be inlined at the point of use. Example:

```scala
object Config {
  inline val logging = false
}

object Logger {

  private var indent = 0

  inline def log[T](msg: String, indentMargin: =>Int)(op: => T): T =
    if (Config.logging) {
      println(s"${"  " * indent}start $msg")
      indent += indentMargin
      val result = op
      indent -= indentMargin
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
way, `inline` is equivalent to Java and Scala 2's `final`. Note that `final`, meaning
_inlined constant_, is still supported in Dotty, but will be phased out.

The `Logger` object contains a definition of the **inline method** `log`. This
method will always be inlined at the point of call.

In the inlined code, an `if-then-else` with a constant condition will be rewritten
to its `then`- or `else`-part. Consequently, in the `log` method above the
`if (Config.loggi0ng)` with `Config.logging == true` will get rewritten into its
`then`-part.

Here's an example:

```scala
var indentSetting = 2

def factorial(n: BigInt): BigInt = {
  log(s"factorial($n)", indentSetting) {
    if (n == 0) 1
    else n * factorial(n - 1)
  }
}
```

If `Config.logging == false`, this will be rewritten (simplified) to

```scala
def factorial(n: BigInt): BigInt = {
  if (n == 0) 1
  else n * factorial(n - 1)
}
```

As you notice, since neither `msg` or `indentMargin` were used, they do not
appear in the generated code for `factorial`. Also note the body of our `log`
method: the `else-` part reduces to just an `op`. In the generated code we do
not generate any closures because we only refer to a by-name parameter *once*.
Consequently, the code was inlined directly and the call was beta-reduced.

In the `true` case the code will be rewritten to:

```scala
def factorial(n: BigInt): BigInt = {
  val msg = s"factorial($n)"
  println(s"${"  " * indent}start $msg")
  Logger.inline$indent_=(indent.+(indentSetting))
  val result =
    if (n == 0) 1
    else n * factorial(n - 1)
  Logger.inline$indent_=(indent.-(indentSetting))
  println(s"${"  " * indent}$msg = $result")
  result
}
```

Note, that the by-value parameter is evaluated only once, per the usual Scala
semantics, by binding the value and reusing the `msg` through the body of
`factorial`. Also, note the special handling of setting to the private var
`indent` by generating the setter method `def inline$indent_=`.

### Recursive Inline Methods

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

<!--- (Commented out since the docs and implementation differ)

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
-->

#### The definition of constant expression

Right-hand sides of inline values and of arguments for inline parameters must be
constant expressions in the sense defined by the [SLS §
6.24](https://www.scala-lang.org/files/archive/spec/2.12/06-expressions.html#constant-expressions),
including _platform-specific_ extensions such as constant folding of pure
numeric computations.

## Specializing Inline (Whitebox)

Inline methods support the ` <: T` return type syntax. This means that the return type
of the inline method is going to be specialized to a more precise type upon
expansion. Example:

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

// obj1.meth() // compile-time error: `meth` is not defined on `A`
obj2.meth()    // OK
```
Here, the inline method `choose` returns an object of either of the two dynamic types
`A` and `B`. If `choose` had been declared with a normal return type `: A`, the result
of its expansion would always be of type `A`, even though the computed value might be
of type `B`. The inline method is "blackbox"  in the sense that details of its
implementation do not leak out. But with the specializing return type `<: A`,
the type of the expansion is the type of the expanded body. If the argument `b`
is `true`, that type is `A`, otherwise it is `B`. Consequently, calling `meth` on `obj2`
type-checks since `obj2` has the same type as the expansion of `choose(false)`, which is `B`.
Inline methods with specializing return types are "whitebox" in that the type
of an application of such a method can be more specialized than its declared
return type, depending on how the method expands.

In the following example, we see how the return type of `zero` is specialized to
the singleton type `0` permitting the addition to be ascribed with the correct
type `1`.

```scala
inline def zero() <: Int = 0

final val one: 1 = zero() + 1
```

## Inline Conditionals

If the condition of an if-then-else expressions is a constant, the expression simplifies to
the selected branch. Prefixing an if-then-else expression with `inline` forces
the condition to be a constant, and thus guarantees that the conditional will always
simplify.

Example:

```scala
inline def update(delta: Int) =
  inline if (delta >= 0) increaseBy(delta)
  else decreaseBy(-delta)
```
A call `update(22)` would rewrite to `increaseBy(22)`. But if `update` was called with
a value that was not a compile-time constant, we would get a compile time error like the one
below:

```scala
   |  inline if (delta >= 0) ???
   |  ^
   |  cannot reduce inline if
   |   its condition
   |     delta >= 0
   |   is not a constant value
   | This location is in code that was inlined at ...
```

## Inline Matches

A `match` expression in the body of an `inline` method definition may be
prefixed by the `inline` modifier. If there is enough static information to
unambiguously take a branch, the expression is reduced to that branch and the
type of the result is taken. If not, a compile-time error is raised that
reports that the match cannot be reduced.

The example below defines an inline method with a
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

## The `scala.compiletime` Package

The `scala.compiletime` package contains helper definitions that provide support for compile time operations over values. They are described in the following.

#### `constValue`, `constValueOpt`, and the `S` combinator

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

#### `erasedValue`

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
`erased`, so can be only used at compile-time during type checking.

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
  case _ => None
}
```

Then:
```scala
  val dInt: Some[Int] = defaultValue[Int]
  val dDouble: Some[Double] = defaultValue[Double]
  val dBoolean: Some[Boolean] = defaultValue[Boolean]
  val dAny: None.type = defaultValue[Any]
```

As another example, consider the type-level version of `toNat` above the we call
`toIntT`: given a _type_ representing a Peano number, return the integer _value_
corresponding to it. Consider the definitions of numbers as in the _Inline
Match_ section aboce. Here's how `toIntT` can be defined:

```scala
inline def toIntT[N <: Nat] <: Int = inline scala.compiletime.erasedValue[N] match {
  case _: Zero.type => 0
  case _: Succ[n] => toIntT[n] + 1
}

final val two = toIntT[Succ[Succ[Zero.type]]]
```

`erasedValue` is an `erased` method so it cannot be used and has no runtime
behavior. Since `toInt` performs static checks over the static type of `N` we
can safely use it to scrutinize its return type (`S[S[Z]]` in this case).

#### `error`

The `error` method is used to produce user-defined compile errors during inline expansion.
It has the following signature:

```scala
inline def error(inline msg: String): Nothing
```

If an inline expansion results in a call `error(msgStr)` the compiler
produces an error message containing the given `msgStr`.

```scala
inline def fail() = {
  error("failed for a reason")
}
fail() // error: failed for a reason
```

or

```scala
inline def fail(p1: => Any) = {
  error(code"failed on: $p1")
}
fail(indentity("foo")) // error: failed on: indentity("foo")
```

## Implicit Matches

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
