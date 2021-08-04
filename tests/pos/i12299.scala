object Outer0 {

  object Inner {
    class Bar(x: Int):
      def this() = this(0)
  }

  export Inner.Bar

  val _ = Bar()
  val _ = Bar(2)

}

object Outer2 {

  object Inner {
    class Bar(x: Int):
      def this() = this(0)
  }

  object test2:
    export Inner._

    val x = Bar()
    val y = Bar(2)

  object test3:
    export Inner.Bar
    def Bar: () => String = () => ""
    val x = Bar()
}

object Outer3 {
  export Outer0._

  private val x = Bar()
  private val y = Bar(2)
}

object Outer4 {

  object Inner {
    class Bar(x: Int):
      def this() = this(0)
    object Bar
  }

  export Inner._

  val _ = Bar()
  val _ = Bar(2)

}
