// Shows that overloading resolution does not test implicits to decide
// applicability. A best alternative is picked first, and then implicits
// are searched for this one.
case class Show[T](val i: Int)
class Show1[T](i: Int) extends Show[T](i)

class Generic
object Generic {
  implicit val gen: Generic = new Generic
  implicit def showGen[T](implicit gen: Generic): Show[T] = new Show[T](2)
}

object Test extends App {
  trait Context
  //implied ctx for Context

  object a {
    def foo[T](implicit gen: Generic): Show[T] = new Show[T](1)
    def foo[T](implicit gen: Generic, ctx: Context): Show1[T] = new Show1[T](2)
  }
  object b {
    def foo[T](implicit gen: Generic): Show[T] = new Show[T](1)
    def foo[T]: Show[T] = new Show[T](2)
  }

  assert(a.foo[Int].i == 2) // error: no implicit argument of type Test.Context was found for parameter ctx
}