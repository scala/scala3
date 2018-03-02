object Test {

  def main(args: Array[String]): Unit = {
    def f(ghost i: Int) = {
      new Foo(foo)(i)
    }
    f(foo)
  }

  def foo: Int = {
    println("foo")
    42
  }

}

class Foo(a: Int)(ghost b: Int) {
  println("Foo")
}
