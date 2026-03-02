// https://github.com/scala/scala3/issues/11681

import scala.collection.Factory

final case class Gen[+A]() {
  def take[C[X] <: Iterable[X], B](
      n: Int
  )(implicit w: A <:< C[B], f: Factory[B, C[B]]): Gen[C[B]] =
    Gen()
}

object Usage {
  def expected: Gen[List[Int]] =
    Gen[List[Int]]().take(3)
}

object example:
  type G[A]
  given G[H[Int]] = ???

  trait H[X]
  object H {
    given H[Int] = ???
  }

  def take[C[_]](using w: G[C[Int]], f: C[Int]) = ???

  def test = take
