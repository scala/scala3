import scala.quoted._

object Test {
  def staged[T](using s: Scope) = {
    import s.tasty._
    given typeT as s.Type[T] // error
    val tTypeTree = typeT
    val tt = Type.of[T]
    '{ "in staged" }
  }

  val s: Scope = ???

  given s.Expr[Int] // error
  new s.Expr[Int] // error
  class Expr2 extends s.Expr[Int] // error

  given s.Type[Int] // error
  new s.Type[Int] // error
  class Type2 extends s.Type[Int] // error

}
