package scala.runtime

import language.experimental.captureChecking

import scala.util.TupledFunction
import scala.annotation.experimental

@experimental
object TupledFunctions {

  def tupledFunction0[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => ((args: EmptyTuple) => f.asInstanceOf[() => Any].apply()).asInstanceOf[G],
    untupledImpl = (g: G) => (() => g.asInstanceOf[EmptyTuple => Any].apply(EmptyTuple)).asInstanceOf[F]
  )

  def tupledFunction1[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => ((args: Tuple1[Any]) => f.asInstanceOf[Any => Any].apply(args._1)).asInstanceOf[G],
    untupledImpl = (g: G) => ((x1: Any) => g.asInstanceOf[Tuple1[?] => Any].apply(Tuple1(x1))).asInstanceOf[F]
  )

  def tupledFunction2[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function2[?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) => Function.untupled(g.asInstanceOf[Tuple2[?, ?] => Any]).asInstanceOf[F]
  )

  def tupledFunction3[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function3[?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) => Function.untupled(g.asInstanceOf[Tuple3[?, ?, ?] => Any]).asInstanceOf[F]
  )

  def tupledFunction4[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function4[?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) => Function.untupled(g.asInstanceOf[Tuple4[?, ?, ?, ?] => Any]).asInstanceOf[F]
  )

  def tupledFunction5[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function5[?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) => Function.untupled(g.asInstanceOf[Tuple5[?, ?, ?, ?, ?] => Any]).asInstanceOf[F]
  )

  def tupledFunction6[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function6[?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any) =>
        g.asInstanceOf[Tuple6[?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6))).asInstanceOf[F]
  )

  def tupledFunction7[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function7[?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any) =>
        g.asInstanceOf[Tuple7[?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7))).asInstanceOf[F]
  )

  def tupledFunction8[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function8[?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any) =>
        g.asInstanceOf[Tuple8[?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8))).asInstanceOf[F]
  )

  def tupledFunction9[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function9[?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any) =>
        g.asInstanceOf[Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9))).asInstanceOf[F]
  )

  def tupledFunction10[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any) =>
        g.asInstanceOf[Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))).asInstanceOf[F]
  )

  def tupledFunction11[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any) =>
        g.asInstanceOf[Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11))).asInstanceOf[F]
  )

  def tupledFunction12[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any) =>
        g.asInstanceOf[Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12))).asInstanceOf[F]
  )

  def tupledFunction13[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any) =>
        g.asInstanceOf[Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13))).asInstanceOf[F]
  )

  def tupledFunction14[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any) =>
        g.asInstanceOf[Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))).asInstanceOf[F]
  )

  def tupledFunction15[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any, x15: Any) =>
        g.asInstanceOf[Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15))).asInstanceOf[F]
  )

  def tupledFunction16[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any, x15: Any, x16: Any) =>
        g.asInstanceOf[Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16))).asInstanceOf[F]
  )

  def tupledFunction17[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any, x15: Any, x16: Any, x17: Any) =>
        g.asInstanceOf[Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17))).asInstanceOf[F]
  )

  def tupledFunction18[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any, x15: Any, x16: Any, x17: Any, x18: Any) =>
        g.asInstanceOf[Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18))).asInstanceOf[F]
  )

  def tupledFunction19[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any, x15: Any, x16: Any, x17: Any, x18: Any, x19: Any) =>
        g.asInstanceOf[Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19))).asInstanceOf[F]
  )

  def tupledFunction20[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any, x15: Any, x16: Any, x17: Any, x18: Any, x19: Any, x20: Any) =>
        g.asInstanceOf[Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20))).asInstanceOf[F]
  )

  def tupledFunction21[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any, x15: Any, x16: Any, x17: Any, x18: Any, x19: Any, x20: Any, x21: Any) =>
        g.asInstanceOf[Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21))).asInstanceOf[F]
  )

  def tupledFunction22[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => f.asInstanceOf[Function22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]].tupled.asInstanceOf[G],
    untupledImpl = (g: G) =>
      ((x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any, x10: Any, x11: Any, x12: Any, x13: Any, x14: Any, x15: Any, x16: Any, x17: Any, x18: Any, x19: Any, x20: Any, x21: Any, x22: Any) =>
        g.asInstanceOf[Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => Any].apply((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22))).asInstanceOf[F]
  )

  def tupledFunctionXXL[F, G]: TupledFunction[F, G] = TupledFunction[F, G](
    tupledImpl = (f: F) => ((args: TupleXXL) => f.asInstanceOf[FunctionXXL].apply(args.elems)).asInstanceOf[G],
    untupledImpl = (g: G) => new FunctionXXL {
      override def apply(xs: IArray[Object]): AnyRef = g.asInstanceOf[TupleXXL => AnyRef].apply(TupleXXL.fromIArray(xs))
    }.asInstanceOf[F]
  )

}
