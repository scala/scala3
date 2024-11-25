import compiletime.ExpressibleAsCollectionLiteral

class A

case class B(xs: Int*) extends A
case class C(xs: Int*) extends A

class D

object SeqLits:

  given [T] => ExpressibleAsCollectionLiteral[B]:
    type Elem = Int
    inline def fromLiteral(inline xs: Int*): B = B(xs*)

  given [T] => ExpressibleAsCollectionLiteral[C]:
    type Elem = Int
    inline def fromLiteral(inline xs: Int*): C = C(xs*)

  val x: A = [1, 2, 3] // error: ambiguous
  val y: D = [1, 2, 3] // error: type mismatch
