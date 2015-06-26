object Test {
  def main(args: Array[String]): Unit = {
    object A {
      def a = new A
      def y = 0
    }
    class A { A.y }
    A.a
  }
}
