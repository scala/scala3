object Test {

  def main(args: Array[String]): Unit = {
    new Foo().apply(foo)
  }

  def foo = {
    println("foo")
    42
  }
}

class Foo extends GhostFunction1[Int, Int] {
  def apply(ghost x: Int): Int = {
    println("Foo.apply")
    42
  }
}
