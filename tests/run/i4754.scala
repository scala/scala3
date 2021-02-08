object Foo {
  private final val x = 1
  private def y = 2
  private def z: 3 = { println("Side effect"); 3 }
}

class Foo {
  import Foo.*
  inline def foo = x + Foo.x + y + Foo.y + z + Foo.z
}

object Test {
  def main(args: Array[String]): Unit = {
    println((new Foo).foo)
  }
}
