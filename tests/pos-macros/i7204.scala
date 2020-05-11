import scala.quoted._

object Foo {
  def impl(using s: Scope) : Unit = {
    import s.tasty._
    val Select(_, _) = (??? : Term)
  }
}
