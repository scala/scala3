object i5068 {
  case class Box[F[_]](value: F[Int])
  sealed trait IsK[F[_], G[_]]
  final case class ReflK[F[_]]() extends IsK[F, F]

  def foo[F[_], G[_]](r: F IsK G, a: Box[F]): Box[G] = r match { case ReflK() => a }

  def main(args: Array[String]): Unit = {
    println(foo(ReflK(), Box(Option(10))))
  }
}
