object Test {

  def main(args: Array[String]): Unit = {
    def f(unused i: Int) = {
      new Foo(i)(foo)
    }
    f(5)
  }

  def foo: Int = {
    println("foo")
    42
  }

}

class Foo(unused a: Int)(b: Int) {
  println("Foo")
}
