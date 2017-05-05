import tests.SpecializeUtils._

object Test {
  def main(args: Array[String]): Unit = {
    val foo = new Foo
    System.out.println(foo.fun1(new Bar))
    System.out.println(foo.fun2(new Bar))

    checkMethodExists(classOf[Foo], "fun1", List(classOf[Foo]), classOf[Foo], specialized = false)
    checkMethodExists(classOf[Foo], "fun1", List(classOf[Bar]), classOf[Bar])

    checkMethodExists(classOf[Foo], "fun2", List(classOf[Foo]), classOf[Foo], specialized = false)
  }

}

class Foo {
  def fun1[F <: Foo](x: F) = x // Specialized
  def fun2(x: Foo) = x // Not specialized

}

class Bar extends Foo {
  override def toString: String = "Bar"
}
