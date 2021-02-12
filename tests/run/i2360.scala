
object Test {
  def main(args: Array[String]): Unit = {
    import Foo.*
    println(foo)
  }
}

object Foo extends Bar {
  inline def foo: Int = bar
}

class Bar {
  def bar = 42
}
