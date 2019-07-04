# Functional Typelevel Programming in Scala

This is a working draft document for discussing language constructs in
support of typelevel programming in Scala 3.

## State of the Art

Currently, typelevel programming in Scala is mainly done using implicits.
Proof search for finding implicit arguments can be used to compute new, interesting types.
This results in a programming style much like Prolog. Amazing feats have
been achieved using this scheme, but things are certainly far from
ideal. In particular:

 - The logic programming style requires a shift of mindset compared to the
   usual functional programming style in Scala.
 - The ways to control implicit search are underdeveloped,
   leading to complicated schemes, requiring rule prioritization or other hacks.
 - Because of their conceptual complexity the resulting typelevel programs are often
   fragile.
 - Error diagnostics are usually very bad, making programming with implicits somewhat
   of a black art. Dotty has greatly improved error dignostics for recursive implicits,
   but the fundamental problem remains.

## The Core Idea

A simple principle underlies the new design: Typelevel programming in Scala 3 means reducing terms and taking their types afterwards. Specifically, if `f` is a _rewrite_ method applied to some arguments `es` then the type of the application `f(es)` is the type of the term to which `f(es)` reduces. Since reduction can specialize types, that type can be more specific than `f`'s declared result type. Type-level programming in Scala 3 is thus a form of partial evaluation or [type specialization](http://www.cse.chalmers.se/~rjmh/Papers/typed-pe.html).

## Rewrite Methods

Consider the standard definition of typelevel Peano numbers:
```scala
trait Nat
case object Z extends Nat
case class S[N <: Nat](n: N) extends Nat
```

A function that maps non-negative integers to Peano numbers can be defined as follows:

```scala
rewrite def toNat(n: Int): Nat = rewrite n match {
  case 0 => Z
  case n if n > 0 => S(toNat(n - 1))
}
```
Without the `rewrite` modifier, an application of `toNat(3)` would have type `Nat`, as this
is the method's declared return type. But with `rewrite`, the call to `toNat(3)` gets reduced _at compile time_ as follows:

        toNat(3)
    ->
        rewrite 3 match {
          case 0 => Z
          case n if n > 0 => S(toNat(n - 1))
        }
    ->
        S(toNat(2))
    ->
        S(rewrite 2 match {
            case 0 => Z
            case n if n > 0 => S(toNat(n - 1))
          })
    ->
        S(S(toNat(1)))
    -> ->
        S(S(S(toNat(0))))
    ->
        S(S(S(rewrite 0 match {
                case 0 => Z
                case n if n > 0 => S(toNat(n - 1))
              })))
    ->
        S(S(S(Z)))

The type of `toNat(3)` is the type of its result, `S[S[S[Z]]]`, which is a subtype of the declared result type `Nat`.

A `rewrite` modifier on a method definition indicates that any application of the defined method outside a rewrite method definition will be expanded to its right hand side, where formal parameters get bound to actual arguments. The right side will be further simplified using a set of rewrite rules given below.

## Rewrite Matches

A match expression in the body of a rewrite meyhod may be prefixed itself with the `rewrite` modifier. Such a rewrite match _must_ be rewritten at compile-time. That is, if there is enough static information to unambiguously pick one of its branches, the expression
is rewritten to that branch. Otherwise a compile-time error is signalled indicating that the match cannot be reduced.

As examples, consider the following two functions over tuples:

```scala
rewrite def concat(xs: Tuple, ys: Tuple): Tuple = rewrite xs match {
  case ()       => ys
  case (x, xs1) => (x, concat(xs1, ys))
}

rewrite def nth(xs: Tuple, n: Int): Any = rewrite xs match {
  case (x, _)   if n == 0 => x
  case (_, xs1) if n > 0  => nth(xs1, n - 1)
}
```
Assume
```scala
as: (Int, String)
bs: (Boolean, List[Int])
tp: Tuple
```

Then we get the following typings:
```scala
concat(as, bs) : (Int, String, Boolean, List[Int])
concat(as, ()) : (Int, String)
concat((), as) : (Int, String)
concat(as, tp) : (Int, (String, Tuple))

nth(as, 0)     : Int
nth(as, 1)     : String
nth(as, 2)     : Nothing
nth(as, -1)    : Nothing
nth(concat(as, bs), 3) : List[Int]
```

In each of these cases, the result is obtained by expanding the rewrite method(s), simplifying (reducing) their right hand sides, and taking the type of the result. As an example, the applications `concat(as, bs)` and `nth(as, 1)` would produce expressions like these:
```scala
concat(as, bs)  -->  (as._1, { val a$1 = as._2; (a$1._1, bs) }
nth(as, 1)      -->  { val a$1 = as._2; a$1._1 }
```
If tuples get large, so do the expansions. For instance, the size of the expansion of a valid selection `nth(xs, n)` is proportional to `n`. We will show later a scheme to avoid this code blowup using `erased` values.

The following expressions would all give compile-time errors since a `rewrite` `match` could not be reduced:
```scala
concat(tp, bs)
nth(tp, 0)
nth(as, 2)
nth(as -1)
```
It's possible to add more cases to a rewrite match, thereby moving an error from compile-time to runtime. For instance, here is a version of `nth` that throws a runtime error in case the index is out of bounds:
```scala
rewrite def nthDynamic(xs: Tuple, n: Int): Any = rewrite xs match {
  case (x, _)   if n == 0 => x
  case (_, xs1) => nthDynamic(xs1, n - 1)
  case () => throw new IndexOutOfBoundsException
}
```
Here is an expansion of `nthDynamic` applied to a tuple `as: (Int, String)` and a negative index. For clarity we have added the computed types of the intermediate values `as$i`.
```
        nthDynamic(as, -1)
    ->
        { val as$0: (String, ()) = as._1
          nthDynamic(as$0, -2)
        }
    ->
        { val as$0: (String, ()) = as._1
          { val as$1: () = as$0._1
            nthDynamic(as$1, -3)
          }
        }
    ->
        throw new IndexOutOfBoundsException
```
So the end result of the expansion is the expression `throw new IndexOutOfBoundsException`, which has type `Nothing`. It is important to note that programs are treated as _data_ in this process, so the exception will not be thrown at compile time, but only if the program is run after it compiles without errors.

## Rewrite Conditionals

Like matches, if-then-else expressions can also be prefixed with the `rewrite` modifier. Such rewrite conditional must have conditions that with a compile-time boolean result. The conditional then recuces to one of its branches depending on whether the condition is known to be `true` or to be `false`.

For instance, here is an alternative version of `toNat` formulated with a conditional instead of a `match`.
```scala
rewrite def toNat(n: Int): Nat =
  rewrite if n == 0 then Z
  else S(toNat(n - 1))
}
```

## Diagnostics

The last definition of `toNat` has a problem. When applied to a negative argument such as `-1` it would lead to an infinitely recurring expansion
```
   toNat(-1) --> toNat(-2) --> toNat(-3) --> ...
```
This will give a compile-time error indicating that the maximal number of allowed expansions is exceeded. The previous `match` based version of `toNat` did not have this problem since we could guard each case with the correct precondition as a guard. So with the first version,`toNat(-1)` would have produced an irreducible match error instead of an infinite expansion error. This is better but ideally we'd like to see a more customized error message instad, such as
```
    Illegal argument for `toNat`: -1 is negative
```
To implement this, there is a predefined method
```scala
rewrite def error(msg: String): Unit
```
in the `typelevel` package. An application of this method will always give a compile-time error with the given string argument as error message. Using this method, `toNat` can be
formulated as follows:
```scala
import scala.typelevel.error
rewrite def toNat(n: Int): Nat =
  rewrite
    if n == 0 then Z
    else if n > 0 then S(toNat(n - 1))
    else error("Illegal argument for `toNat`: $n is negative")
}
```

## Rewrite Rules

The following rewrite rules are performed when simplifying inlined bodies:

 - **Pattern matching:** Pattern matches in `rewrite match` expressions are evaluated and the case with the first pattern that is statically known to match is chosen. A pattern match `rewrite E match { Case_1 ... Case_n }` is translated as follows:

   - We first create a binding `val $scrutinee_n = E` for the scrutinee of the match.
   - Matching a pattern `P` takes as additional input the current scrutinee reference `S` (this is `$scrutinee_n` for toplevel patterns). It either returns with a sequence of additional bindings or fails. The rules depend on the form of the pattern `P`.

     - If `P` is a type test `Q: T`, the match fails if `S.type` does not conform to `T`.
       Otherwise, proceed by matching `Q` against `S`.
     - If `P` is a wildcard `_`, the match succeeds with no additional bindings.
     - If `P` is some other pattern variable `x`, succeed with the additional binding `x = S`.
     - If `P` is a variable binding `x @ Q`, proceed by matching `Q` against `S`. If that
       succeeds, succeed with the additional binding `x = S`.
     - If `P` is some non-variable reference `R`, the match succeeds with no additional bindings
       if we statically know that `S` refers to the same value as `R`.
     - If `P` is a constructor pattern `C(Q_1, ..., Q_n)`, where `C` refers to a case class with a compiler-generated
       `unapply` method, the match fails if `S.type` does not conform to `C`.
       Otherwise, proceed by matching each sub-pattern `Q_i` against the scrutinee `S._i`. If all sub-matches
       succeed, collect the resulting bindings in order.
     - If `P` is a pattern with a guard `Q if E`, the match succeeds if matching `Q` succeeds and
       the guard `E` is statically known to be `true`.

   The first case `case P => E` with a matching pattern `P` is chosen. The match is then rewritten to `{ Bs; E }` where `Bs` are the bindings generated by the match of `P`.

   Note that matches are only evaluated if they are marked `rewrite`.

 - **Reduction of projections:** An expression `E_0(E_1, ..., E_i, ...E_n).f_i`, where

    - `E_0` is known to be - or to forward to - a class instance creation `new C`,
    - `C` is known not to have initialization code,
    - `f_i` is known to select the `i`'th field of `C`,

   is rewritten to `E_i`. If the prefix `E_0` or any of the other arguments `E_j` have side effects, they are
   executed in order before the result of `E_i` is returned.

 - **Conditional simplification:** If the condition of an if-then-else is statically known, the conditional
   is rewritten to its then- or else-part, depending on the known value of the condition. If the condition
   is pure, it is dropped; otherwise the selected branch is preceded by evaluation of the condition, in order
   not to drop any side effects.

 - **Reduction of function applications:** An application of a lambda `((x_1, ..., x_n) => B)(E_1, ..., E_n)`
   is rewritten to

        { val/def x_1 = E_1
          ...
          val/def x_n = E_n
          B
        }

   where `val`s are used for value parameter bindings and `def`s are used for by-name parameter bindings.
   If an argument `E_i` is a simple variable reference `y`, the corresponding binding is omitted and `y`
   is used instead of `x_i` in `B`.

 - **Constant folding:** If a pure expression evaluates to a constant value, it can be replaced by this value.

 - **Garbage collection:** Bindings for parameters and pattern-bound variables are dropped according to the
   following rules:

   - A binding `val x = E` is dropped if `x` is not used in the rest of the expansion and `E` is pure.
   - A binding `def x = E` for a by-name parameter is dropped if `x` is not used in the rest of the expansion.
   - A binding `def x = E` for a by-name parameter that is used exactly once in the result of the
     expansion is dropped and the reference to `x` is replaced by `E`.

   Dropping a binding might make other bindings redundant. Garbage collection proceeds until no further bindings
   can be dropped.

All rules are sound wrt to the language semantics. However, it should be noted that rewriting a `rewrite match` at compile time does not guarantee to pick the same case as every execution at runtime of the same match without the `rewrite`. Here is an example:
```scala
trait A
trait B

rewrite def f(x: Any) = rewrite x match {
  case _: A => "A"
  case _: B => "B"
}
def g(x: Any) = x match {
  case _: A => "A"
  case _: B => "B"
}

val b1: B = new B {}
val b2: B = new A with B {}

f(b1)  // rewrites to "B"
g(b1)  // evaluates to "B"
f(b2)  // rewrites to "B"
g(b2)  // evaluates to "A"
```
Since both applications of `f` are passed arguments with static type `B`, the second case `"B"` is chosen in each case. By contrast, the application `g(b2)` yields `"A"`, since
even though the static type of `b2` is `B`, its runtime value is of type `A & B`, which
matches the first case.

## Restrictions for Rewrite Methods

Rewrite methods are effectively final; they may not be overwritten. Rewrite methods
must satisfy two more restrictions, which ensure that no code needs to be generated for them.

 1. They must be always fully applied.
 2. They may override other methods only if one of the overridden methods is concrete.

The right hand side of a rewrite method is never invoked by dynamic dispatch. As an example consider a situation like the following:
```scala
class Iterable[T] {
  def foreach(f: T => Unit): Unit = ...
}
class List[T] extends Iterable[T] {
  override rewrite def foreach(f: T => Unit): Unit = ...
}
val xs: Iterable[T] = ...
val ys: List[T] = ...
val zs: Iterable[T] = ys
xs.foreach(f)  // calls Iterable's foreach
ys.foreach(f)  // expands to the body of List's foreach
zs.foreach(f)  // calls Iterable's foreach
```
It follows that an overriding rewrite method should implement exactly the same semantics as the method it overrides (but possibly more efficiently).

## Matching on Types

We have seen so far rewrite methods that take terms (tuples and integers) as parameters. What if we want to base case distinctions on types instead? For instance, one would like to be able to write a function `defaultValue`, that, given a type `T`
returns optionally the default value of `T`, if it exists. In fact, we can already express
this using rewrite match expressions and a simple helper function, `scala.typelevel.erasedValue`, which is defined as follows:
```scala
erased def erasedValue[T]: T = ???
```
The `erasedValue` function _pretends_ to return a value of its type argument `T`. In fact, it would always raise a `NotImplementedError` exception when called. But the function can in fact never be called, since it is declared `erased`, so can be only used a compile-time during type checking.

Using `erasedValue`, we can then define `defaultValue` as follows:
```scala
rewrite def defaultValue[T] = rewrite erasedValue[T] match {
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
As another example, consider the type-level inverse of `toNat`: given a _type_ representing a Peano number, return the integer _value_ corresponding to it. Here's how this can be defined:
```scala
rewrite def toInt[N <: Nat]: Int = rewrite erasedValue[N] match {
  case _: Z => 0
  case _: S[n] => toInt[n] + 1
}
```

## Computing New Types

The examples so far all computed _terms_ that have interesting new types. Since in Scala terms can contain types it is equally easy to compute the types of these terms directly as well. To this purpose, it is helpful
to base oneself on the helper class `scala.typelevel.Typed`, defined as follows:
```scala
class Typed[T](val value: T) { type Type = T }
```
Here is a version of `concat` that computes at the same time a result and its type:
```scala
rewrite def concatTyped(xs: Tuple, ys: Tuple): Typed[_ <: Tuple] = rewrite xs match {
  case ()       => Typed(ys)
  case (x, xs1) => Typed((x, concatTyped(xs1, ys).value))
}
```

## Avoiding Code Explosion

Recursive rewrite methods implement a form of loop unrolling through inlining. This can lead to very large generated expressions. The code explosion can be avoided by calling typed versions of the rewrite methods to define erased values, of which  just the typed part is used afterwards. Here is how this can be done for `concat`:

```scala
def concatImpl(xs: Tuple, ys: Tuple): Tuple = xs match {
  case ()       => ys
  case (x, xs1) => (x, concatImpl(xs1, ys))
}

rewrite def concat(xs: Tuple, ys: Tuple): Tuple = {
  erased val resTpe = concatTyped(xs, ys)
  concatImpl(xs, ys).asInstanceOf[resTpe.Type]
}
```
The `concat` rewrite method makes use of two helper functions, `concatTyped` (described in the last section) and `concatImpl`. `concatTyped` is called as the right hand side of an `erased` value `resTpe`. Since `resTpe` is `erased`, no code is generated for its definition.
`concatImpl` is a regular, non-rewrite method that implements `concat` on generic tuples. It is not inlineable, and its result type is always `Tuple`. The actual code for `concat` calls `concatImpl` and casts its result to type `resTpe.Type`, the computed result type of the concatenation. This gives the best of both worlds: Compact code and expressive types.

One might criticize that this scheme involves code duplication. In the example above, the recursive `concat` algorithm had to be implemented twice, once as a regular method, the other time as a rewrite method. However, in practice it is is quite likely that the regular function would use optimized data representatons and algorithms that do not lend themselves easily to a typelevel interpretation. In these cases a dual implementation is required anyway.

## Code Specialization

Rewrite methods are a convenient means to achieve code specialization. As an example, consider implementing a math library that implements (among others) a `dotProduct` method. Here is a possible implementation of `MathLib` with some user code.
```scala
abstract class MathLib[N : Numeric] {
  def dotProduct(xs: Array[N], ys: Array[N]): N
}
object MathLib {

  rewrite def apply[N](implicit n: Numeric[N]) = new MathLib[N] {
    import n._
    def dotProduct(xs: Array[N], ys: Array[N]): N = {
      require(xs.length == ys.length)
      var i = 0
      var s: N = n.zero
      while (i < xs.length) {
        s = s + xs(i) * ys(i)
        i += 1
      }
      s
    }
  }
}
```
`MathLib` is intentionally kept very abstract - it works for any element type `N` for which a `math.Numeric` implementation exists.
Here is some code that uses `MathLib`:
```scala
val mlib = MathLib[Double]

val xs = Array(1.0, 1.0)
val ys = Array(2.0, -3.0)
mlib.dotProduct(xs, ys)
```
The implementation code for a given numeric type `N` is produced by the `apply` method of `MathLib`.
Even though the `dotProduct` code looks simple, there is a lot of hidden complexity in the generic code:

 - It uses unbounded generic arrays, which means code on the JVM needs reflection to access their elements
 - All operations on array elements forward to generic operations in class `Numeric`.

It would be quite hard for even a good optimizer to undo the generic abstractions and replace them with something simpler if `N` is specialized to a primitive type like `Double`. But since `apply` is a rewrite method, the specialization happens automatically as a result of inlining the body of `apply` with the new types. Indeed, the specialized version of `dotProduct` looks like this:
```
def dotProduct(xs: Array[Double], ys: Array[Double]): Double = {
  require(xs.length == ys.length)
  var i = 0
  var s: Double = math.Numeric.DoubleIsFractional.zero
  while (i < xs.length) {
    s = s + xs(i) * ys(i)
    i += 1
  }
  s
}
```
In other words, specialization with rewrite methods allows "abstraction without regret". The price to pay for this is the increase of code size through inlining. That price is often worth paying, but inlining decisions need to be considered carefully.

## Implicit Matches

It is foreseen that many areas of typelevel programming can be done with rewrite methods instead of implicits. But sometimes implicits are unavoidable. The problem so far was that the Prolog-like programming style of implicit search becomes viral: Once some construct depends on implicit search it has to be written as a logic program itself. Consider for instance the problem
of creating a `TreeSet[T]` or a `HashSet[T]` depending on whether `T` has an `Ordering` or not. We can create a set of implicit definitions like this:
```scala
trait SetFor[T, S <: Set[T]]
class LowPriority {
  implicit def hashSetFor[T]: SetFor[T, HashSet[T]] = ...
}
object SetsFor extends LowPriority {
  implicit def treeSetFor[T: Ordering]: SetFor[T, TreeSet[T]] = ...
}
```
Clearly, this is not pretty. Besides all the usual indirection of implicit search,
we face the problem of rule prioritization where
we have to ensure that `treeSetFor` takes priority over `hashSetFor` if the element type has an ordering. This is solved
(clumsily) by putting `hashSetFor` in a superclass `LowPriority` of the object `SetsFor` where `treeSetFor` is defined.
Maybe the boilerplate would still be acceptable if the crufty code could be contained. However, this is not the case. Every user of
the abstraction has to be parameterized itself with a `SetFor` implicit. Considering the simple task _"I want a `TreeSet[T]` if
`T` has an ordering and a `HashSet[T]` otherwise"_, this seems like a lot of ceremony.

There are some proposals to improve the situation in specific areas, for instance by allowing more elaborate schemes to specify priorities. But they all keep the viral nature of implicit search programs based on logic programming.

By contrast, the new `implicit match` construct makes implicit search available in a functional context. To solve
the problem of creating the right set, one would use it as follows:
```scala
rewrite def setFor[T]: Set[T] = implicit match {
  case ord: Ordering[T] => new TreeSet[T]
  case _                => new HashSet[T]
}
```
An implicit match uses the `implicit` keyword in the place of the scrutinee. Its patterns are type ascriptions of the form `identifier : Type`.

Patterns are tried in sequence. The first case with a pattern `x: T` such that an implicit value
of type `T` can be summoned is chosen. The variable `x` is then bound to the implicit value for the remainder of the case. It can in turn be used as an implicit in the right hand side of the case.
It is an error if one of the tested patterns gives rise to an ambiguous implicit search.

An implicit matches is considered to be a special kind of a rewrite match. This means it can only occur in the body of a rewrite method, and it must be reduced at compile time.

## Transparent Methods

Recall that rewrite methods are always erased. Sometimes, this is inconvenient. Consider for example a type with a user-defined `unapply` method. If the method is marked as `rewrite`
pattern reduction can "see inside" the method by expanding it. But the extractor can then not be used at run-time. To allow both use cases at the same time, one can use the `transparent` modifier. E.g.
```scala
transparent def unapply(x: C) = ...
```
Transparent methods are regular methods that can be called at runtime. But when an application of a transparent method occurs in the inlined body of a rewrite method,
the method is itself inlined, just like if it had been marked `rewrite`.

## Transparent Values

Value definitions can also be marked `inline`. Examples:
```scala
inline val label      = "url"
inline val pi: Double = 3.14159265359
```
The right hand side of a  `inline` value definition must be a pure expression of constant type. The type of the value is then the type of its right hand side, without any widenings. For instance, the type of `label` above is the singleton type `"url"` instead of `String` and the type of `pi` is `3.14159265359` instead of `Double`.

Transparent values are effectively final; they may not be overridden. In Scala-2, constant values had to be expressed using `final`, which gave an unfortunate double meaning to the modifier. The `final` syntax is still supported in Scala 3 for a limited time to support cross-building.

The `inline` modifier can also be used for value parameters of `rewrite` methods. Example
```scala
rewrite def power(x: Double, inline n: Int) = ...
```
If a `inline` modifier is given, corresponding arguments must be pure expressions of constant type.

However, the restrictions on right-hand sides or arguments mentioned in this section do not apply in code that is
itself in a `rewrite` method. In other words, constantness checking is performed only when a `transparent` method
is expanded, not when it is defined.

## Rewrite and Inline

Dotty up to version 0.9 also supports an `inline` modifier. `inline` is similar to `rewrite` in that inlining happens during type-checking. However, `inline` does not change the types of the inlined expressions. The expressions are instead inlined in the form of trees that are already typed.

Since there is large overlap between `rewrite` and `inline`, we have dropped `inline` as a separate modifier. The current `@inline` annotation, which represents a hint to the optimizer that inlining might be advisable, remains unaffected.

## Relationship to "TypeOf"

This document describes a scheme to implement typelevel computations using rewrite methods.  An alternative approach is explored in #4671 where instead of rewrite methods one has
transparent methods that can specialize types without rewriting terms. The main difference is that the present proposal uses the machinery of partial evaluation (PE) whereas #4671 uses the machinery of (term-) dependent types (DT).

The PE approach reduces the original right-hand side of a rewrite method via term rewriting. The right hand side is conceptually the term as it is written down, closed over the environment where the rewrite method was defined. This is implemented by rewriting and then re-type-checking the original untyped tree, but with typed leaves that refer to the tree's free variables. The re-type-checking with more specific argument types can lead to a type derivation that is quite different
than the original typing of the rewrite method's right hand side. Types might be inferred to be more specific, overloading resolution could pick different alternatives, and implicit searches might yield different results or might be elided altogether.
This flexibility is what makes code specialization work smoothly.

By constrast, the DT approach typechecks the right hand side of a transparent function in a special way. Instead of
computing types as usual, it "lifts" every term into a type that mirrors it. To do this, it needs to create type equivalents
of all relevant term forms including applications, conditionals, match expressions, and the various forms of pattern matches.
The end result of the DT approach is then a type `{t}` that represents essentially the right-hand side term `t` itself, or at least all portions that might contribute to that type.  The type has to be constructed in such a way that the result type of every application
of a transparent function can be read off from it.
An application of a transparent function then needs to just instantiate that type, whereas the term is not affected at all, and the function is called as usual. This means there is a guarantee that the
semantics of a transparent call is the same as a normal call, only the type is better. On the other hand, one cannot play
tricks such as code specialization, since there is nothing to specialize.
Also, implicit matches would not fit into this scheme, as they require term rewritings. Another concern is how much additional complexity would be caused by mirroring the necessary term forms in types and tweaking type inference to work with the new types.

To summarize, here are some advantages of DT over PE:

 + It avoids code explosion by design (can be solved in PE but requires extra work).
 + Expansion only works on types, so this might well be faster at compile-time than PE's term rewriting.
 + It gives a guarantee that `transparent` is semantics preserving.
 + The typeof `{t}` operator is interesting in its own right.

By contrast, here are some advantages of PE over DT:

 + It uses the standard type checker and typing rules with no need to go to full dependent types
   (only path dependent types instead of full term-dependencies).
 + It simplify more cases.
 + It can do code specialization.
 + It can do implicit matches.
 + It can also serve as the inlining mechanism for implementing macros with staging (see next section).

## Relationship To Meta Programming

Rewrite methods are a safer alternative to the whitebox macros in Scala 2. Both share the principle that some function applications get expanded such that the types of these applications are the types of their expansions. But the way the expansions are performed is completely different: For rewrite methods, expansions are simply beta reduction (_aka_ inlining) and a set of rewrite rules that are patterned after the language semantics. All rewritings are performed by the compiler itself. It is really as if some parts of the program reduction are performed early, which is the essence of partial evaluation.

By contrast, Scala 2 macros mean that user-defined code is invoked to process program fragments as data. The result of this computation is then embedded instead of the macro call. Macros are thus a lot more powerful than rewrite methods, but also a lot less safe.

Functionality analogous to blackbox macros in Scala-2 is available in Scala-3 through its [principled meta programming](reference/metaprogramming/toc.md) system: Code can be turned into data using quotes `(')`. Code-returning computations can be inserted into quotes using splices `(~)`. A splice outside quotes means that the spliced computation is _run_, which is the analogue of
invoking a macro. Quoted code can be inspected using reflection on Tasty trees.

To compare: here's the scheme used in Scala-2 to define a macro:
```scala
def m(x: T) = macro mImpl
...
def mImpl(x: Tree) = ...
```
Here's analogous functionality in Scala-3:
```scala
rewrite def m(x: T) = ~mImpl('x)
...
def mImpl(x: Expr[T]) = ...
```
The new scheme is more explicit and orthogonal: The usual way to define a macro is as a rewrite method that invokes with `~` a macro library method, passing it quoted arguments as data. The role of the rewrite method is simply to avoid to have to write the splices and quotes in user code.

One important difference between the two schemes is that the reflective call implied by `~` is performed _after_ the program is typechecked. Besides a better separation of concerns, this also provides a more predictable environment for macro code to run in.

## Acknowledgments

Many of the ideas in this proposal resulted from discussions with @gsps and @OlivierBlanvillain, the authors of the "TypeOf" approach (PR #4671). @gsps suggested the use of the `transparent` keyword. @OlivierBlanvillain suggested techniques like `erasedValue` and `Typed` to lift term computations to types. The present proposal also benefited from feedback from @milessabin, @adriaanm, @sjrd, Andrei Alexandrescu, John Hughes, Conor McBride and Stephanie Weirich on earlier designs. The relationship with meta programming has a lot in common with the original inline and meta proposals in SIP 28 and SIP 29.
