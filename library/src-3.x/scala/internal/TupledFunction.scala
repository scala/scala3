package scala.internal

object TupledFunction {

  def tupledFunction0[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G =
      ((args: Unit) => f.asInstanceOf[() => Any].apply()).asInstanceOf[G]
  }

  def tupledFunction1[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G =
      ((args: Tuple1[Any]) => f.asInstanceOf[Any => Any].apply(args._1)).asInstanceOf[G]
  }

  def tupledFunction2[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function2[_, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction3[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function3[_, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction4[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function4[_, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction5[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function5[_, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction6[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function6[_, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction7[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function7[_, _, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction8[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function8[_, _, _, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction9[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function9[_, _, _, _, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction10[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function10[_, _, _, _, _, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction11[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function11[_, _, _, _, _, _, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction12[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function12[_, _, _, _, _, _, _, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction13[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function13[_, _, _, _, _, _, _, _, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction14[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction15[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction16[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction17[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction18[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction19[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction20[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction21[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunction22[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G = f.asInstanceOf[Function22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].tupled.asInstanceOf[G]
  }

  def tupledFunctionXXL[F, G]: TupledFunction[F, G] = new TupledFunction {
    def apply(f: F): G =
      ((args: TupleXXL) => f.asInstanceOf[FunctionXXL].apply(args.elems)).asInstanceOf[G]
  }

}
