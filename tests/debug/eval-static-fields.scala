object Test:
  def main(args: Array[String]): Unit =
    example.A.m()

package example:
  object A:
    def m(): Unit =
      println("A.m()" + a2)
      B.m()

    val a1 = "a1"
    private val a2 = "a2"
    private[example] val a3 = "a3"

    private object B:
      def m(): Unit = println("B.m()")
      val b1 = "b1"
      private val b2 = "b2"
      private[A] val b3 = "b3"
      override def toString: String = "example.A.B"

    private[A] object C:
      val c1 = "c1"

    object D:
      val d1 = "d1"

  private object E:
    val e1 = "e1"
    override def toString: String = "example.E"
