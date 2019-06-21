object PostConditions {
  opaque type WrappedResult[T] = T

  def result[T] given (r: WrappedResult[T]): T = r

  def (x: T) ensuring [T](condition: given WrappedResult[T] => Boolean): T = {
    delegate for WrappedResult[T] = x
    assert(condition)
    x
  }
}

object Test {
  import PostConditions.{ensuring, result}
  val s = List(1, 2, 3).sum.ensuring(result == 6)
}