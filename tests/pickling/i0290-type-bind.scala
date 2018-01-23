object foo{
  val x = List(1,2,3)
  x match {
    case t: List[tt] => t.head.asInstanceOf[tt]
  }
}

object bar {

  class C[T <: Seq[_]]

  val x: AnyRef = new C

  x match {
    case x: C[u] =>
      def x: u = x
      val s: Seq[_] = x
  }
}

object foo2{{
  val x = List(1,2,3)
  x match {
    case t: List[tt] => t.head.asInstanceOf[tt]
  }
}}

object bar2 {{

  class C[T <: Seq[_]]

  val x: AnyRef = new C

  x match {
    case x: C[u] =>
      def x: u = x
      val s: Seq[_] = x
  }
}}

object foo3{ val x0 = {
  val x = List(1,2,3)
  x match {
    case t: List[tt] => t.head.asInstanceOf[tt]
  }
}}

object bar3 { def f0 = {

  class C[T <: Seq[_]]

  val x: AnyRef = new C

  x match {
    case x: C[u] =>
      def x: u = x
      val s: Seq[_] = x
  }
}}
