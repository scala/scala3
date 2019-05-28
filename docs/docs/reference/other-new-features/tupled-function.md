---
layout: doc-page
title: "Tupled Function"
---

Tupled Function
----------------------

With functions bounded to arities up to 22 it was possible to generalize some operation on all function types using overloading. 
Now that we have functions and tuples generalized to [arities above 22](https://dotty.epfl.ch/docs/reference/dropped-features/limit22.html) overloading is not an option anymore. 
The type class `TupleFunction` provides a way to abstract directly over functions of any arity converting it to an equivalent function that receives all arguments in a single tuple.

```scala
/** Type class relating a `FunctionN[..., R]` with an equvalent tupled function `Function1[TupleN[...], R]`
 *
 *  @tparam F a function type
 *  @tparam G a tupled function type (function of arity 1 receiving a tuple as argument)
 */
@implicitNotFound("${F} cannot be tupled as ${G}")
trait TupledFunction[F, G] {
  def apply(f: F): G
}
```


Examples
--------
`TupledFunction` can be used to generalize the `Tuple2.tupled`, ... `Tuple22.tupled` method to functions of any arities ([full example](https://github.com/lampepfl/dotty/tests/run/tupled-function-tupled.scala))

```scala
/** Creates a tupled version of this function: instead of N arguments,
 *  it accepts a single [[scala.Tuple]] argument.
 *
 *  @tparam F the function type
 *  @tparam Args the tuple type with the same types as the function arguments of F
 *  @tparam R the return type of F
 */
def (f: F) tupled[F, Args <: Tuple, R] given (tupled: TupledFunction[F, Args => R]): Args => R = tupled(f)
```

`TupledFunction` can also be used to generalize the [`Tuple1.compose`](https://github.com/lampepfl/dotty/tests/run/tupled-function-compose.scala) and [`Tuple1.andThen`](https://github.com/lampepfl/dotty/tests/run/tupled-function-andThen.scala) methods to compose functions of larger arities and with functions that return tuples.

```scala
/** Composes two instances of TupledFunctions in a new TupledFunctions, with this function applied last
 *
 *  @tparam F a function type
 *  @tparam G a function type
 *  @tparam FArgs the tuple type with the same types as the function arguments of F and return type of G
 *  @tparam GArgs the tuple type with the same types as the function arguments of G
 *  @tparam R the return type of F
 */
def (f: F) compose[F, G, FArgs <: Tuple, GArgs <: Tuple, R](g: G) given (tupledG: TupledFunction[G, GArgs => FArgs], tupledF: TupledFunction[F, FArgs => R]): GArgs => R = {
  (x: GArgs) => tupledF(f)(tupledG(g)(x))
}
```
