//> using options -language:experimental.erasedDefinitions

object Test {

  def main(args: Array[String]): Unit = {
    new Foo().apply(foo)
  }

  def foo = {
    println("foo")
    42
  }
}

class Foo extends PolyFunction { // error
  def apply(erased x: Int): Int = {
    println("Foo.apply")
    42
  }
}
