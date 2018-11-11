case class Foo(x: 1, y: implicit Int => Int)
object Test {
  val f = Foo(1, implicit (i: Int) => i)
  val fx1: 1 = f.x
  val fx2: 1 = f._1
  val fy1: Int = f.y(1)
  val fy2: Int = f._2(1)
}
