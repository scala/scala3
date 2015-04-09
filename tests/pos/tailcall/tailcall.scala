class tailcall {
  val shift = 1
  final def fact(x: Int, acc: Int = 1): Int = if (x == 0) acc else fact(x - shift, acc * x)
  def id[T <: AnyRef](x: T): T = if (x eq null) x else id(x)
}

class TypedApply[T2]{
  private def firstDiff[T <: TypedApply[T2]](xs: List[T]): Int = firstDiff(xs)
}
