object PostConditions:
  opaque type WrappedResult[T] = T

  def result[T](using r: WrappedResult[T]): T = r

  extension [T](x: T) def ensuring (condition: WrappedResult[T] ?=> Boolean): T =
    given WrappedResult[T] = x
    assert(condition)
    x

object Test:
  import PostConditions.{ensuring, result}
  val s = List(1, 2, 3).sum.ensuring(result == 6)
