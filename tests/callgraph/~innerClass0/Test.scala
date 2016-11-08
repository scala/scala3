
object Test {
  def main(args: Array[String]): Unit = {
    class Foo {
      def bar: Int = 42
    }

    val foo = new Foo
    System.out.println(foo.bar)
  }
}
