trait Zero[F[_]]:
  def zero[A]: F[A]

given Zero[List]:
  def zero[A] = List.empty[A]

given Zero[Option]:
  def zero[A] = Option.empty[A]


@main def Test =
  val test = [F[_]] => (f: Zero[F]) ?=> [G[_]] => (g: Zero[G]) ?=> println("foo")
  test[List][Option]