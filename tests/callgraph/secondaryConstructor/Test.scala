
object Test {
  def main(args: Array[String]): Unit = {
    new Foo()
  }
}

class Foo(n: Int) {
  def this() = this(42)

  def foo() = System.out.println(n)
  foo()
}
