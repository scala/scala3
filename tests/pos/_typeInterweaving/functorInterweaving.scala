//taken from https://dotty.epfl.ch/docs/reference/contextual/type-classes.html
//at version 3.1.1-RC1-bin-20210930-01f040b-NIGHTLY
//modified to have type currying
trait Functor[F[_]]:
  def map[A][B](x: F[A], f: A => B): F[B]


given Functor[List] with
  def map[A](x: List[A])[B](f: A => B): List[B] =
    x.map(f)

def assertTransformation[F[_]: Functor][A][B](expected: F[B], original: F[A], mapping: A => B): Unit =
  assert(expected == summon[Functor[F]].map(original, mapping))

@main def test =
  assertTransformation(List("a1", "b1"), List("a", "b"), elt => s"${elt}1")
