class X extends Object

object Implicits {

  implicit val impl: X = new X

  val a: Object = "abc"
  val b: Any = "abc"

  def foo(x: Int)(implicit y: X): Int = {
    println(y)
    x
  }

  val y: Int = foo(1)

}
