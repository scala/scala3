object Test {

  def main(args: Array[String]): Unit = {
    def f erased (i: Int) = {
      new Foo(i)(foo)
    }
    f(5)
  }

  def foo: Int = {
    println("foo")
    42
  }

}

class Foo erased (a: Int)(b: Int) {
  println("Foo")
}
