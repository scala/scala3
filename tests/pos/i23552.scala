import NamedTuple.AnyNamedTuple

sealed trait Z
sealed trait S[n]

type TupleList[+A, N] <: AnyNamedTuple =
  N match
    case Z => NamedTuple.Empty
    case S[n] => (head: A, tail: TupleList[A, n])

sealed trait Vect[+A, N]:
  def ::[A1 >: A](a: A1): Vect[A1, S[N]] =
    Vect.Cons(a, this)

  def toTupleList: TupleList[A, N]

object Vect:
  case object Empty extends Vect[Nothing, Z]:
    override def toTupleList: TupleList[Nothing, Z] = NamedTuple.Empty

  case class Cons[A, N](head: A, tail: Vect[A, N]) extends Vect[A, S[N]]:
    override def toTupleList: TupleList[A, S[N]] = (head, tail.toTupleList)

object Foo:
  def unapply[A, N](as: Vect[A, N]): Some[TupleList[A, N]] =
    Some(as.toTupleList)

@main
def test: Unit =
  (1 :: 2 :: 3 :: Vect.Empty) match
    // missing parens around named tuple inside Foo causes compiler crash
    case Foo(head = h, tail = t) => ??? // error