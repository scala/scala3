object Test:
  def main(args: Array[String]): Unit =
    example.A.m()

package example:
  object A:
    def m(): Unit =
      println("A.m()")
      B.m()

    def a1(str: String) = s"a1: $str"
    private def a2(str: String) = s"a2: $str"

    private object B:
      def m(): Unit =
        println("B.m()")

      val b1 = "b1"
      def b1(str: String) = s"b1: $str"
      private[A] def b2(str: String) = s"b2: $str"
      private def b3(str: String) = s"b3: $str"

  object C:
    def c1(str: String) = s"c1: $str"
    private[example] def c2(str: String) = s"c2: $str"
