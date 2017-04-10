
object Test {
  def main(args: Array[String]): Unit = {
    val foo = new Foo
    System.out.println(foo.f(42))
    System.out.println(foo.f("string"))
    System.out.println(foo.f(foo))

    val methods = classOf[Foo].getDeclaredMethods
    assert(methods.count(x => x.getName.matches("f") && x.getReturnType == classOf[Object]) == 1)
    assert(methods.count(x => x.getName.matches("f\\$spec\\d*") && x.getReturnType == classOf[Int]) == 1)
    assert(methods.count(x => x.getName.matches("f\\$spec\\d*") && x.getReturnType == classOf[String]) == 1)
    assert(methods.count(x => x.getName.matches("f\\$spec\\d*") && x.getReturnType == classOf[Foo]) == 1)
  }
}

class Foo {
  def f[T](e: T): T = e

  override def toString: String = "Foo"
}
