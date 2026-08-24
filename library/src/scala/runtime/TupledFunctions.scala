package scala.runtime

import language.experimental.captureChecking

import scala.util.TupledFunction
import scala.annotation.experimental

@experimental
object TupledFunctions {

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 0
   *  can be tupled as `G`, the unary function type taking `EmptyTuple` as its
   *  argument.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 0; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 0
   *  @tparam G the tupled form of `F`, mapping `EmptyTuple` to `F`'s result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction0[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => ((args: EmptyTuple) => f.asInstanceOf[() => Any].apply()).asInstanceOf[G],
    untupledImpl = (g: G) => (() => g.asInstanceOf[EmptyTuple => Any].apply(EmptyTuple)).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 1
   *  can be tupled as `G`, the unary function type taking the single argument
   *  of `F` wrapped in a `Tuple1`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 1; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 1
   *  @tparam G the tupled form of `F`, mapping a `Tuple1` of `F`'s argument to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction1[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => ((args: Tuple1[Any]) => f.asInstanceOf[Any => Any].apply(args._1)).asInstanceOf[G],
    untupledImpl = (g: G) => ((x1: Any) => g.asInstanceOf[Tuple1[?] => Any].apply(Tuple1(x1))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 2
   *  can be tupled as `G`, the unary function type taking the 2 arguments of
   *  `F` as a single `Tuple2`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 2; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 2
   *  @tparam G the tupled form of `F`, mapping a `Tuple2` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction2[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function2[?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) => Function.untupled(g.asInstanceOf[Tuple2[?, ?] => Any]).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 3
   *  can be tupled as `G`, the unary function type taking the 3 arguments of
   *  `F` as a single `Tuple3`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 3; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 3
   *  @tparam G the tupled form of `F`, mapping a `Tuple3` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction3[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function3[?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) => Function.untupled(g.asInstanceOf[Tuple3[?, ?, ?] => Any]).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 4
   *  can be tupled as `G`, the unary function type taking the 4 arguments of
   *  `F` as a single `Tuple4`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 4; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 4
   *  @tparam G the tupled form of `F`, mapping a `Tuple4` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction4[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function4[?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) => Function.untupled(g.asInstanceOf[Tuple4[?, ?, ?, ?] => Any]).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 5
   *  can be tupled as `G`, the unary function type taking the 5 arguments of
   *  `F` as a single `Tuple5`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 5; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 5
   *  @tparam G the tupled form of `F`, mapping a `Tuple5` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction5[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function5[?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) => Function.untupled(g.asInstanceOf[Tuple5[?, ?, ?, ?, ?] => Any]).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 6
   *  can be tupled as `G`, the unary function type taking the 6 arguments of
   *  `F` as a single `Tuple6`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 6; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 6
   *  @tparam G the tupled form of `F`, mapping a `Tuple6` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction6[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function6[?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any) =>
        g.asInstanceOf[Tuple6[?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 7
   *  can be tupled as `G`, the unary function type taking the 7 arguments of
   *  `F` as a single `Tuple7`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 7; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 7
   *  @tparam G the tupled form of `F`, mapping a `Tuple7` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction7[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function7[?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any) =>
        g.asInstanceOf[Tuple7[?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 8
   *  can be tupled as `G`, the unary function type taking the 8 arguments of
   *  `F` as a single `Tuple8`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 8; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 8
   *  @tparam G the tupled form of `F`, mapping a `Tuple8` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction8[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function8[?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any) =>
        g.asInstanceOf[Tuple8[?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 9
   *  can be tupled as `G`, the unary function type taking the 9 arguments of
   *  `F` as a single `Tuple9`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 9; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 9
   *  @tparam G the tupled form of `F`, mapping a `Tuple9` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction9[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function9[?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any) =>
        g.asInstanceOf[Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 10
   *  can be tupled as `G`, the unary function type taking the 10 arguments of
   *  `F` as a single `Tuple10`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 10; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 10
   *  @tparam G the tupled form of `F`, mapping a `Tuple10` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction10[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any) =>
        g.asInstanceOf[Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 11
   *  can be tupled as `G`, the unary function type taking the 11 arguments of
   *  `F` as a single `Tuple11`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 11; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 11
   *  @tparam G the tupled form of `F`, mapping a `Tuple11` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction11[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any) =>
        g.asInstanceOf[Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 12
   *  can be tupled as `G`, the unary function type taking the 12 arguments of
   *  `F` as a single `Tuple12`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 12; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 12
   *  @tparam G the tupled form of `F`, mapping a `Tuple12` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction12[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any) =>
        g.asInstanceOf[Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 13
   *  can be tupled as `G`, the unary function type taking the 13 arguments of
   *  `F` as a single `Tuple13`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 13; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 13
   *  @tparam G the tupled form of `F`, mapping a `Tuple13` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction13[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any) =>
        g.asInstanceOf[Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 14
   *  can be tupled as `G`, the unary function type taking the 14 arguments of
   *  `F` as a single `Tuple14`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 14; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 14
   *  @tparam G the tupled form of `F`, mapping a `Tuple14` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction14[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any) =>
        g.asInstanceOf[Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 15
   *  can be tupled as `G`, the unary function type taking the 15 arguments of
   *  `F` as a single `Tuple15`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 15; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 15
   *  @tparam G the tupled form of `F`, mapping a `Tuple15` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction15[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any, x15: Any) =>
        g.asInstanceOf[Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 16
   *  can be tupled as `G`, the unary function type taking the 16 arguments of
   *  `F` as a single `Tuple16`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 16; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 16
   *  @tparam G the tupled form of `F`, mapping a `Tuple16` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction16[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any, x15: Any, x16: Any) =>
        g.asInstanceOf[Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 17
   *  can be tupled as `G`, the unary function type taking the 17 arguments of
   *  `F` as a single `Tuple17`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 17; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 17
   *  @tparam G the tupled form of `F`, mapping a `Tuple17` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction17[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any, x15: Any, x16: Any, x17: Any) =>
        g.asInstanceOf[Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 18
   *  can be tupled as `G`, the unary function type taking the 18 arguments of
   *  `F` as a single `Tuple18`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 18; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 18
   *  @tparam G the tupled form of `F`, mapping a `Tuple18` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction18[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any, x15: Any, x16: Any, x17: Any, x18: Any) =>
        g.asInstanceOf[Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 19
   *  can be tupled as `G`, the unary function type taking the 19 arguments of
   *  `F` as a single `Tuple19`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 19; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 19
   *  @tparam G the tupled form of `F`, mapping a `Tuple19` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction19[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any, x15: Any, x16: Any, x17: Any, x18: Any, x19: Any) =>
        g.asInstanceOf[Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 20
   *  can be tupled as `G`, the unary function type taking the 20 arguments of
   *  `F` as a single `Tuple20`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 20; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 20
   *  @tparam G the tupled form of `F`, mapping a `Tuple20` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction20[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any, x15: Any, x16: Any, x17: Any, x18: Any, x19: Any, x20: Any) =>
        g.asInstanceOf[Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 21
   *  can be tupled as `G`, the unary function type taking the 21 arguments of
   *  `F` as a single `Tuple21`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 21; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 21
   *  @tparam G the tupled form of `F`, mapping a `Tuple21` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction21[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any, x15: Any, x16: Any, x17: Any, x18: Any, x19: Any, x20: Any, x21: Any) =>
        g.asInstanceOf[Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity 22
   *  can be tupled as `G`, the unary function type taking the 22 arguments of
   *  `F` as a single `Tuple22`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity 22; it is not meant to
   *  be called directly. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity 22
   *  @tparam G the tupled form of `F`, mapping a `Tuple22` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunction22[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any, x15: Any, x16: Any, x17: Any, x18: Any, x19: Any, x20: Any, x21: Any, x22: Any) =>
        g.asInstanceOf[Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22))).asInstanceOf[F]
  )

  /** Returns a `TupledFunction` witnessing that a function type `F` of arity
   *  greater than 22 can be tupled as `G`, the unary function type taking the
   *  arguments of `F` as a single `TupleXXL`.
   *
   *  The compiler synthesizes calls to this method to materialize a given
   *  `TupledFunction[F, G]` for function types of arity greater than 22; it is
   *  not meant to be called directly. Unlike the fixed-arity variants, both
   *  conversions work on array-based runtime representations: `tupled` turns a
   *  [[FunctionXXL]] into a function that applies it to the `TupleXXL`
   *  argument's backing array of elements, and `untupled` builds a new
   *  `FunctionXXL` whose `apply` wraps its argument array in a `TupleXXL`
   *  (sharing the array, not copying it) and passes that tuple to the
   *  converted function. `F` and `G` are unconstrained here: the conversions
   *  are implemented with unchecked casts, so type arguments of any other
   *  shape lead to `ClassCastException`s when the conversions or the converted
   *  functions are applied.
   *
   *  @tparam F the function type of arity greater than 22, represented at runtime by [[FunctionXXL]]
   *  @tparam G the tupled form of `F`, mapping a `TupleXXL` of `F`'s arguments to its result
   *  @return a `TupledFunction` whose `tupled` converts an `F` into a `G` and whose `untupled` converts a `G` into an `F`
   */
  def tupledFunctionXXL[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => ((args: TupleXXL) => f.asInstanceOf[FunctionXXL].apply(args.elems)).asInstanceOf[G],
    untupledImpl = (g: G) => new FunctionXXL {
      override def apply(xs: IArray[Object]): AnyRef = g.asInstanceOf[TupleXXL => AnyRef].apply(TupleXXL.fromIArray(xs))
    }.asInstanceOf[F]
  )

}
