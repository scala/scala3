object Test {

  def main(args: Array[String]): Unit = {
    def f erased (i: Int) = {
      new Foo(foo)(i)
    }
    f(foo)
  }

  def foo: Int = {
    println("foo")
    42
  }

}

class Foo(a: Int) erased (b: Int) {
  println("Foo")
}
