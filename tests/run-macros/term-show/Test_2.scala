object Test {
  import TypeToolbox.*
  def main(args: Array[String]): Unit = {
   assert(show {
      class C {
        def a = 0
        private def b = 0
        private[this] def c = 0
        private[C] def d = 0
        protected def e = 0
        protected[this] def f = 0
        protected[C] def g = 0
      }
    }
    ==
    """{
      |  class C() {
      |    def a: scala.Int = 0
      |    private[this] def b: scala.Int = 0
      |    private[this] def c: scala.Int = 0
      |    private[C] def d: scala.Int = 0
      |    protected def e: scala.Int = 0
      |    protected[this] def f: scala.Int = 0
      |    protected[C] def g: scala.Int = 0
      |  }
      |  ()
      |}""".stripMargin)
  }
}
