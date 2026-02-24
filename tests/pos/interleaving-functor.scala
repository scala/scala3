
object functorInterleaving:
  //taken from https://nightly.scala-lang.org/docs/reference/contextual/type-classes.html
  //at version 3.1.1-RC1-bin-20210930-01f040b-NIGHTLY
  //modified to have type interleaving
  trait Functor[F[_]]:
    def map[A](x: F[A])[B](f: A => B): F[B]


  given Functor[List]:
    def map[A](x: List[A])[B](f: A => B): List[B] =
      x.map(f)

  def assertTransformation[F[_]: Functor, A](original: F[A])[B](expected: F[B])(mapping: A => B): Unit =
    assert(expected == summon[Functor[F]].map(original)(mapping))

  @main def testInterweaving =
    assertTransformation(List("a", "b"))(List("a1", "b1")){elt => s"${elt}1"}
