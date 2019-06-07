object Test {

  def main(args: Array[String]): Unit = {
    new Foo(foo)
  }

  def foo: Int = {
    println("foo")
    42
  }

}

class Foo erased (a: Int) {
  println("Foo")
}
