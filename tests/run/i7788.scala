trait Show[-A]:
  def show(a:A): String

given Show[String] = x => x
given Show[Int] = _.toString

given showEither: [A,B] => (sA: Show[A]) => Show[B] => Show[Either[A,B]] =
  _.fold(a => s"Left(${summon[Show[A]].show(a)})", b => s"Right(${summon[Show[B]].show(b)})")
given [A,B] => (sA: Show[A]) => (sB: Show[B]) => Show[(A,B)] = (a,b) => s"(${sA.show(a)}), ${sB.show(b)})"


@main def Test =
  println(summon[Show[(Int, String)]].show(0 -> "hello"))
  println(summon[Show[Either[Int, String]]].show(Left(-1)))
  println(summon[Show[Either[Int, String]]].show(Right("success message")))
