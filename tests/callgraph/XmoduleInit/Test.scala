
object Foo {
  def foo() = System.out.println(42)
  foo()
}

object Test {
  def main(args: Array[String]): Unit = {
    Foo
  }
}
