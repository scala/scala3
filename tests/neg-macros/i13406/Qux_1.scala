// Qux_1.scala
sealed trait Qux[T] // anonymous mirror because no companion
object QuxImpl:
  case class Foo[A](a: A) extends Qux[A]
