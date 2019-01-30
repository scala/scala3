case class Foo(x: 1, y: given Int => Int)
object Test {
  val f = Foo(1, given (i: Int) => i)
  val fx1: 1 = f.x
  val fx2: 1 = f._1
  val fy1: Int = f.y given 1
  val fy2: Int = f._2 given 1
}
