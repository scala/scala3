package scala.runtime

@deprecated("Use scala.runtime.Tuple", "")
object DynamicTuple {

  inline val MaxSpecialized = 22

  @deprecated("use toArray", "")
  def dynamicToArray(self: Tuple): Array[Object] = scala.runtime.Tuple.toArray(self)

  @deprecated("use toIArray", "")
  def dynamicToIArray(self: Tuple): IArray[Object] = scala.runtime.Tuple.toIArray(self)

  @deprecated("Use fromArray")
  def dynamicFromArray[T <: Tuple](xs: Array[Object]): T = scala.runtime.Tuple.fromArray(xs).asInstanceOf[T]

  @deprecated("Use fromIArray", "")
  def dynamicFromIArray[T <: Tuple](xs: IArray[Object]): T = scala.runtime.Tuple.fromIArray(xs).asInstanceOf[T]

  @deprecated("Use fromProduct", "")
  def dynamicFromProduct[T <: Tuple](xs: Product): T = scala.runtime.Tuple.fromProduct(xs).asInstanceOf[T]

  @deprecated("Use cons", "")
  def dynamicCons[H, This <: Tuple](x: H, self: This): H *: This = scala.runtime.Tuple.cons(x, self).asInstanceOf[H *: This]

  @deprecated("Use concat", "")
  def dynamicConcat[This <: Tuple, That <: Tuple](self: This, that: That): scala.Tuple.Concat[This, That] = scala.runtime.Tuple.concat(self, that).asInstanceOf[scala.Tuple.Concat[This, That]]

  @deprecated("Use size", "")
  def dynamicSize[This <: Tuple](self: This): scala.Tuple.Size[This] = scala.runtime.Tuple.size(self).asInstanceOf[scala.Tuple.Size[This]]

  @deprecated("Use tail", "")
  def dynamicTail[This <: NonEmptyTuple](self: This): scala.Tuple.Tail[This] = scala.runtime.Tuple.tail(self).asInstanceOf[scala.Tuple.Tail[This]]

  @deprecated("Use apply", "")
  def dynamicApply[This <: NonEmptyTuple, N <: Int](self: This, n: N): scala.Tuple.Elem[This, N] = scala.runtime.Tuple.apply(self, n).asInstanceOf[scala.Tuple.Elem[This, N]]

  @deprecated("Use zip", "")
  def dynamicZip[This <: Tuple, T2 <: Tuple](t1: This, t2: T2): scala.Tuple.Zip[This, T2] = scala.runtime.Tuple.zip(t1, t2).asInstanceOf[scala.Tuple.Zip[This, T2]]

  @deprecated("Use map", "")
  def dynamicMap[This <: Tuple, F[_]](self: This, f: [t] => t => F[t]): scala.Tuple.Map[This, F] = scala.runtime.Tuple.map(self, f).asInstanceOf[scala.Tuple.Map[This, F]]

  @deprecated("Use take", "")
  def dynamicTake[This <: Tuple, N <: Int](self: This, n: N): scala.Tuple.Take[This, N] = scala.runtime.Tuple.take(self, n).asInstanceOf[scala.Tuple.Take[This, N]]

  @deprecated("Use drop", "")
  def dynamicDrop[This <: Tuple, N <: Int](self: This, n: N): scala.Tuple.Drop[This, N] = scala.runtime.Tuple.drop(self, n).asInstanceOf[scala.Tuple.Drop[This, N]]

  @deprecated("Use splitAt", "")
  def dynamicSplitAt[This <: Tuple, N <: Int](self: This, n: N): scala.Tuple.Split[This, N] = scala.runtime.Tuple.splitAt(self, n).asInstanceOf[scala.Tuple.Split[This, N]]

  def consIterator(head: Any, tail: Tuple): Iterator[Any] =
    Iterator.single(head) ++ tail.asInstanceOf[Product].productIterator

  def concatIterator(tup1: Tuple, tup2: Tuple): Iterator[Any] =
    tup1.asInstanceOf[Product].productIterator ++ tup2.asInstanceOf[Product].productIterator

}
