object Test {

  def main(args: Array[String]): Unit = {
    new Foo().apply(foo)
  }

  def foo = {
    println("foo")
    42
  }
}

class Foo extends UnusedFunction1[Int, Int] {
  def apply(unused x: Int): Int = {
    println("Foo.apply")
    42
  }
}
