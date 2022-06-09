import deriving.Mirror

object MirrorK1:
  type Of[F[_]] = Mirror { type MirroredType[A] = F[A] }

sealed trait Box[T]
object Box

case class Child[T]() extends Box[T]

sealed abstract class Foo[T]
object Foo {
  case class A[T]() extends Foo[T]
}

val foo = summon[Mirror.Of[Box[Int] | Box[Int]]] // error
val bar = summon[MirrorK1.Of[[X] =>> Box[Int] | Box[Int]]] // error
def baz = summon[deriving.Mirror.Of[Foo[String] | Foo[String]]] // error

def qux = summon[deriving.Mirror.Of[Option[Int] | Option[String]]] // error
