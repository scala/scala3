object Test {
  case class Tuple2K[F[_], G[_], A](f: F[A], g: G[A])

  val p0: Tuple2K[[X] =>> Int, [X] =>> String, Any] = Tuple2K(1, "s")
  p0 == Tuple2K(List(1), Option("s"))
}
