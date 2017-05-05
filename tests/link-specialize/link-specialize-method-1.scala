import tests.SpecializeUtils._

object Test {
  def main(args: Array[String]): Unit = {
    val foo = new Foo
    System.out.println(foo.f(42))
    System.out.println(foo.f("string"))
    System.out.println(foo.f(foo))

    checkMethodExists(classOf[Foo], "f", List(classOf[Object]), classOf[Object], specialized = false)
    checkMethodExists(classOf[Foo], "f", List(classOf[Int]), classOf[Int])
    checkMethodExists(classOf[Foo], "f", List(classOf[String]), classOf[String])
    checkMethodExists(classOf[Foo], "f", List(classOf[Foo]), classOf[Foo])
  }
}

class Foo {
  def f[T](e: T): T = e

  override def toString: String = "Foo"
}
