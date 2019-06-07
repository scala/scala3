case class Tuple2K[X, A[_], B[_]](a: A[X], b: B[X])

object Test {
  def f[X](x: Tuple2K[X, Option, [Y] =>> Tuple2K[Y, Option, Option]]): Any =
    x match {
      case Tuple2K(_, Tuple2K(_, _)) => ???
    }
}
