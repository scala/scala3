case class Foo1(x: Int) extends Serializable with Serializable // error
case class Foo2(x: Int) extends scala.Serializable with Serializable // error
