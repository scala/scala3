
object Test {
  def main(args: Array[String]): Unit = {
    new Bar().test()
  }
}

class Bar {
  def test() = {
    class Foo {
      def bar: Int = 42
    }

    val foo = new Foo
    System.out.println(foo.bar)
  }
}
