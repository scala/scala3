object PostConditions {
  opaque type WrappedResult[T] = T

  private object WrappedResult {
    def wrap[T](x: T): WrappedResult[T] = x
    def unwrap[T](x: WrappedResult[T]): T = x
  }

  def result[T] given (r: WrappedResult[T]): T = WrappedResult.unwrap(r)

  def (x: T) ensuring [T](condition: given WrappedResult[T] => Boolean): T = {
    implied for WrappedResult[T] = WrappedResult.wrap(x)
    assert(condition)
    x
  }
}

object Test {
  import PostConditions.{ensuring, result}
  val s = List(1, 2, 3).sum.ensuring(result == 6)
}