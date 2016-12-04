object Test {
  def main(args: Array[String]): Unit = {
    object Foo {
      def test() = 42
    }
    System.out.println(Foo.test())
  }
}
