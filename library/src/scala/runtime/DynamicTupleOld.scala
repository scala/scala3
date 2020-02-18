package scala.runtime

@deprecated("Use scala.runtime.Tuple", "")
object DynamicTuple {
  inline val MaxSpecialized = 22

  @deprecated("use toArray", "")
  def dynamicToArray(self: Tuple): Array[Object] = ???

  @deprecated("use toIArray", "")
  def dynamicToIArray(self: Tuple): IArray[Object] = ???

  @deprecated("Use fromArray")
  def dynamicFromArray[T <: Tuple](xs: Array[Object]): T = ???

  @deprecated("Use fromIArray", "")
  def dynamicFromIArray[T <: Tuple](xs: IArray[Object]): T = ???

  @deprecated("Use fromProduct", "")
  def dynamicFromProduct[T <: Tuple](xs: Product): T = ???

  @deprecated("Use cons", "")
  def dynamicCons[H, This <: Tuple](x: H, self: This): H *: This = ???

  @deprecated("Use concat", "")
  def dynamicConcat[This <: Tuple, That <: Tuple](self: This, that: That): scala.Tuple.Concat[This, That] = ???

  def concat(self: Unit | Product, that: Unit | Product): Unit | Product = ???

  @deprecated("Use size", "")
  def dynamicSize[This <: Tuple](self: This): scala.Tuple.Size[This] = ???

  @deprecated("Use tail", "")
  def dynamicTail[This <: NonEmptyTuple](self: This): scala.Tuple.Tail[This] = ???

  @deprecated("Use apply", "")
  def dynamicApply[This <: NonEmptyTuple, N <: Int](self: This, n: N): scala.Tuple.Elem[This, N] = ???

  @deprecated("Use zip", "")
  def dynamicZip[This <: Tuple, T2 <: Tuple](t1: This, t2: T2): scala.Tuple.Zip[This, T2] = ???

  @deprecated("Use map", "")
  def dynamicMap[This <: Tuple, F[_]](self: This, f: [t] => t => F[t]): scala.Tuple.Map[This, F] = ???

  @deprecated("Use take", "")
  def dynamicTake[This <: Tuple, N <: Int](self: This, n: N): scala.Tuple.Take[This, N] = ???

  @deprecated("Use drop", "")
  def dynamicDrop[This <: Tuple, N <: Int](self: This, n: N): scala.Tuple.Drop[This, N] = ???

  @deprecated("Use splitAt", "")
  def dynamicSplitAt[This <: Tuple, N <: Int](self: This, n: N): scala.Tuple.Split[This, N] = ???

  def consIterator(head: Any, tail: Unit | Product): Iterator[Any] = ???

  def concatIterator(tup1: Unit | Product, tup2: Unit | Product): Iterator[Any] = ???

}
