object Test extends App {
  inline def foo(ys: Int*): Unit = bar(ys*)
  def bar(ys: Int*) = ()

  val xs: Array[Int] = new Array[Int](3)
  foo(xs*)

  val ys: Seq[Int] = new Array[Int](3)
  foo(ys*)
}
