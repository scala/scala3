
object Test {
  def main(args: Array[String]): Unit = {
    val foo = new Foo
    println(foo.f(42))
    // TODO check if specialized println(a.getClass.get.getMethod("f").getReturnType)
  }
}

class Foo {
  def f[T](e: T): T = e
}
