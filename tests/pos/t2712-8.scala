class Two[A, B]
class One[A] extends Two[A, A]

object Test {
  def foo[F[_, _]](x: F[Int, Int]) = x

  val t: One[Int] = ???
  foo(t)
}
