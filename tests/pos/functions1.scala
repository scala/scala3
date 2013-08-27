class X(val elem: Int) extends Object {
  def foo(y: String): Int = y.length + elem
}

object Functions {

  val x = new X(2)
  val xe = x.elem
  val xf: String => Int = x.foo(_)
  val x2: String => Int = x.foo
  val x3 = x.foo _

}
