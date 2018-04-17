sealed trait Foo[A]
case class One[A]() extends Foo[A]
sealed abstract case class Bar[A]() extends Foo[A]
class Two() extends Bar[String]

object Test {
  def test(x: Foo[Int]) = x match {
    case One() =>
  }
}
