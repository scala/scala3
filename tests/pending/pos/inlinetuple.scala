// TODO: Ensure that this inlines properly. So far only
// x._1 inlines, but not x._2.
object Test:

  def g(x: Int, y: Int) = x + y
  inline def f(inline x: (Int, Int)) = g(x._1, x._2)

  val x = f((1, 2))

