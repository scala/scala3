package scala.internal

object TupledFunction {

  def tupledFunction0[R]: TupledFunction[() => R, Unit, R] = new TupledFunction {
    def applyFunctionTo(f: () => R, args: Unit): R = f()
  }

  def tupledFunction1[T1, R]: TupledFunction[T1 => R, Tuple1[T1], R] = new TupledFunction {
    def applyFunctionTo(f: T1 => R, args: Tuple1[T1]): R = f(args._1)
  }

  def tupledFunction2[T1, T2, R]: TupledFunction[(T1, T2) => R, Tuple2[T1, T2], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2) => R, args: Tuple2[T1, T2]): R = f(args._1, args._2)
  }

  def tupledFunction3[T1, T2, T3, R]: TupledFunction[(T1, T2, T3) => R, Tuple3[T1, T2, T3], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3) => R, args: Tuple3[T1, T2, T3]): R = f(args._1, args._2, args._3)
  }

  def tupledFunction4[T1, T2, T3, T4, R]: TupledFunction[(T1, T2, T3, T4) => R, Tuple4[T1, T2, T3, T4], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4) => R, args: Tuple4[T1, T2, T3, T4]): R = f(args._1, args._2, args._3, args._4)
  }

  def tupledFunction5[T1, T2, T3, T4, T5, R]: TupledFunction[(T1, T2, T3, T4, T5) => R, Tuple5[T1, T2, T3, T4, T5], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5) => R, args: Tuple5[T1, T2, T3, T4, T5]): R = f(args._1, args._2, args._3, args._4, args._5)
  }

  def tupledFunction6[T1, T2, T3, T4, T5, T6, R]: TupledFunction[(T1, T2, T3, T4, T5, T6) => R, Tuple6[T1, T2, T3, T4, T5, T6], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6) => R, args: Tuple6[T1, T2, T3, T4, T5, T6]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6)
  }

  def tupledFunction7[T1, T2, T3, T4, T5, T6, T7, R]: TupledFunction[(T1, T2, T3, T4, T5, T6, T7) => R, Tuple7[T1, T2, T3, T4, T5, T6, T7], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6, T7) => R, args: Tuple7[T1, T2, T3, T4, T5, T6, T7]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6, args._7)
  }

  def tupledFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R]: TupledFunction[(T1, T2, T3, T4, T5, T6, T7, T8) => R, Tuple8[T1, T2, T3, T4, T5, T6, T7, T8], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6, T7, T8) => R, args: Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6, args._7, args._8)
  }

  def tupledFunction9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]: TupledFunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9) => R, Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R, args: Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6, args._7, args._8, args._9)
  }

  def tupledFunction10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]: TupledFunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R, Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R, args: Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6, args._7, args._8, args._9, args._10)
  }

  def tupledFunction11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]: TupledFunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R, Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R, args: Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6, args._7, args._8, args._9, args._10, args._11)
  }

  def tupledFunction12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]: TupledFunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R, Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R, args: Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6, args._7, args._8, args._9, args._10, args._11, args._12)
  }

  def tupledFunction13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]: TupledFunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R, Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R, args: Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6, args._7, args._8, args._9, args._10, args._11, args._12, args._13)
  }

  def tupledFunction14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]: TupledFunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R, Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R, args: Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6, args._7, args._8, args._9, args._10, args._11, args._12, args._13, args._14)
  }

  def tupledFunction15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]: TupledFunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R, Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R, args: Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6, args._7, args._8, args._9, args._10, args._11, args._12, args._13, args._14, args._15)
  }

  def tupledFunction16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]: TupledFunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R, Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R, args: Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6, args._7, args._8, args._9, args._10, args._11, args._12, args._13, args._14, args._15, args._16)
  }

  def tupledFunction17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]: TupledFunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R, Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R, args: Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6, args._7, args._8, args._9, args._10, args._11, args._12, args._13, args._14, args._15, args._16, args._17)
  }

  def tupledFunction18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]: TupledFunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R, Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R, args: Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6, args._7, args._8, args._9, args._10, args._11, args._12, args._13, args._14, args._15, args._16, args._17, args._18)
  }

  def tupledFunction19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]: TupledFunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R, Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R, args: Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6, args._7, args._8, args._9, args._10, args._11, args._12, args._13, args._14, args._15, args._16, args._17, args._18, args._19)
  }

  def tupledFunction20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]: TupledFunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R, Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R, args: Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6, args._7, args._8, args._9, args._10, args._11, args._12, args._13, args._14, args._15, args._16, args._17, args._18, args._19, args._20)
  }

  def tupledFunction21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]: TupledFunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R, Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R, args: Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6, args._7, args._8, args._9, args._10, args._11, args._12, args._13, args._14, args._15, args._16, args._17, args._18, args._19, args._20, args._21)
  }

  def tupledFunction22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]: TupledFunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R, Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22], R] = new TupledFunction {
    def applyFunctionTo(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R, args: Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]): R =
      f(args._1, args._2, args._3, args._4, args._5, args._6, args._7, args._8, args._9, args._10, args._11, args._12, args._13, args._14, args._15, args._16, args._17, args._18, args._19, args._20, args._21, args._22)
  }

  def tupledFunctionXXL[F, Args <: Tuple, R]: TupledFunction[F, Args, R] = new TupledFunction {
    def applyFunctionTo(f: F, args: Args): R =
      f.asInstanceOf[FunctionXXL].apply(args.asInstanceOf[TupleXXL].elems).asInstanceOf[R]
  }

}
