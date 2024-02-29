def toCsvFlat[A <: Product](a: A)(using m: scala.deriving.Mirror.ProductOf[A]) = {
  def flatTuple(any: Any): Tuple = any match
    case p: Product => p.productIterator.map(flatTuple).foldLeft(EmptyTuple: Tuple)(_ ++ _)
    case a          => Tuple1(a)

  val tuple = flatTuple(Tuple.fromProductTyped(a)).toList
}
