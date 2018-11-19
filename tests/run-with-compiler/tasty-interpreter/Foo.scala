object Foo {
  def main(args: Array[String]): Unit = {
      val x1 = 42
      println(x1)

      lazy val x2 = println("Hello")
      x2
      x2

      def x3 = 42
      println(x3)
  }
}
