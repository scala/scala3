object bar {

  class C[T <: Seq[_]]

  val x: AnyRef = new C

  val y = x match {
    case x: C[u] =>
      def xx: u = xx
      xx
  }

  val z= {
    def xx: String = xx
    xx
  }
}
