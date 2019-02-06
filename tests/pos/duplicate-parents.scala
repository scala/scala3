case class Foo1(x: Int) extends Serializable
case class Foo2(x: Int) extends scala.Serializable
case class Foo3(x: Int) extends Product

case object Foo4 extends Serializable

object Scope {
  class Serializable
  case class Foo5(x: Int) extends Serializable

  val f = Foo5(1)
  f: Scope.Serializable
  f: scala.Serializable
}
