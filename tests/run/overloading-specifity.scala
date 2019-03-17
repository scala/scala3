// Shows that now implicit parameters act as a tie-breaker.
// The alternative with fewer implicit parameters wins.
case class Show[T](val i: Int)

class Generic
object Generic {
  implicit val gen: Generic = new Generic
  implicit def showGen[T](implicit gen: Generic): Show[T] = new Show[T](2)
}

object Test extends App {
  trait Context
  implied ctx for Context

  object a {
    def foo[T](implicit gen: Generic): Show[T] = new Show[T](1)
    def foo[T](implicit gen: Generic, ctx: Context): Show[T] = new Show[T](2)
  }
  object b {
    def foo[T](implicit gen: Generic): Show[T] = new Show[T](1)
    def foo[T]: Show[T] = new Show[T](2)
  }

  assert(a.foo[Int].i == 1)
  assert(b.foo[Int].i == 2)

  def f: Int = 1
  def f(x: Int): Int = x

  val x = f
  val x1: Int = x
}