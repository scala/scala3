object PostConditions with
  opaque type WrappedResult[T] = T

  def result[T](given r: WrappedResult[T]): T = r

  def [T](x: T) ensuring (condition: (given WrappedResult[T]) => Boolean): T =
    given WrappedResult[T] = x
    assert(condition)
    x

object Test with
  import PostConditions.{ensuring, result}
  val s = List(1, 2, 3).sum.ensuring(result == 6)
