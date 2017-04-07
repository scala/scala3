
object Test {
  def main(args: Array[String]): Unit = {
    val a = new Foo(42)
    println(a.e)
    // TODO check if specialized println(a.getClass.get.getMethod("e").getReturnType)
  }
}

class Foo[T](val e: T)
