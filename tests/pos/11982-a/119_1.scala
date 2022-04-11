object Unpair {
  class Inv[T]

  type Head[X] = X match {
    case Tuple2[a, b] => a
  }

  def unpair[X <: Tuple2[Any, Any]]: Head[X] = ???
}
