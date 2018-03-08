object bar {

  class C[T <: Seq[_]]

  val x: AnyRef = new C

  val y = x match {
    case x: C[type U] =>
      def xx: U = xx
      xx
  }

  val z= {
    def xx: String = xx
    xx
  }
}
