---
layout: doc-page
title: "Tupled Function"
---

Tupled Function
----------------------

With functions bounded to arities up to 22 it was possible to generalize some operation on all function types using overloading.
Now that we have functions and tuples generalized to [arities above 22](../dropped-features/limit22.md) overloading is not an option anymore.
The type class `TupleFunction` provides a way to abstract directly over a function of any arity converting it to an equivalent function that receives all arguments in a single tuple.

```scala
/** Type class relating a `FunctionN[..., R]` with an equivalent tupled function `Function1[TupleN[...], R]`
 *
 *  @tparam F a function type
 *  @tparam G a tupled function type (function of arity 1 receiving a tuple as argument)
 */
@implicitNotFound("${F} cannot be tupled as ${G}")
sealed trait TupledFunction[F, G] {
  def tupled(f: F): G
  def untupled(g: G): F
}
```

The compiler will synthesize an instance of `TupledFunction[F, G]` if:

* `F` is a function type of arity `N`
* `G` is a function with a single tuple argument of size `N` and its types are equal to the arguments of `F`
* The return type of `F` is equal to the return type of `G`
* `F` and `G` are the same sort of function (both are `(...) => R` or both are `(...) ?=> R`)
* If only one of `F` or `G` is instantiated the second one is inferred.

Examples
--------
`TupledFunction` can be used to generalize the `Function1.tupled`, ... `Function22.tupled` methods to functions of any arities.
The following defines `tupled` as [extension method](../contextual/extension-methods.html) ([full example](https://github.com/lampepfl/dotty/blob/main/tests/run/tupled-function-tupled.scala)).

```scala
/** Creates a tupled version of this function: instead of N arguments,
 *  it accepts a single [[scala.Tuple]] with N elements as argument.
 *
 *  @tparam F the function type
 *  @tparam Args the tuple type with the same types as the function arguments of F
 *  @tparam R the return type of F
 */
extension [F, Args <: Tuple, R](f: F)
  def tupled(using tf: TupledFunction[F, Args => R]): Args => R = tf.tupled(f)
```

`TupledFunction` can be used to generalize the `Function.untupled` to a function of any arities ([full example](https://github.com/lampepfl/dotty/blob/main/tests/run/tupled-function-untupled.scala))

```scala
/** Creates an untupled version of this function: instead of a single argument of type [[scala.Tuple]] with N elements,
 *  it accepts N arguments.
 *
 *  This is a generalization of [[scala.Function.untupled]] that work on functions of any arity
 *
 *  @tparam F the function type
 *  @tparam Args the tuple type with the same types as the function arguments of F
 *  @tparam R the return type of F
 */
extension [F, Args <: Tuple, R](f: Args => R)
  def untupled(using tf: TupledFunction[F, Args => R]): F = tf.untupled(f)
```

`TupledFunction` can also be used to generalize the [`Tuple1.compose`](https://github.com/lampepfl/dotty/blob/main/tests/run/tupled-function-compose.scala) and [`Tuple1.andThen`](https://github.com/lampepfl/dotty/blob/main/tests/run/tupled-function-andThen.scala) methods to compose functions of larger arities and with functions that return tuples.

```scala
/** Composes two instances of TupledFunction into a new TupledFunction, with this function applied last.
 *
 *  @tparam F a function type
 *  @tparam G a function type
 *  @tparam FArgs the tuple type with the same types as the function arguments of F and return type of G
 *  @tparam GArgs the tuple type with the same types as the function arguments of G
 *  @tparam R the return type of F
 */
extension [F, G, FArgs <: Tuple, GArgs <: Tuple, R](f: F)
  def compose(g: G)(using tg: TupledFunction[G, GArgs => FArgs], tf: TupledFunction[F, FArgs => R]): GArgs => R = {
  (x: GArgs) => tf.tupled(f)(tg.tupled(g)(x))
}
```
