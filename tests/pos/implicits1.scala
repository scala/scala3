class X(elem: Int) extends Object

object Implicits {

  implicit val impl: X = new X(0)

  implicit def conv(x: Int): X = new X(x)

  val a: Object = "abc"
  val b: Any = "abc"

  def foo(x: Int)(implicit y: X): Int = {
    println(y)
    x
  }

  val y: Int = foo(1)

  val z: X = 3

}
