import scala.quoted._

object Test {
  def staged[T](using Quotes) = {
    import qctx.reflect._
    given typeT as Type[T] // error
    val tt = TypeRepr.of[T]
    '{ "in staged" }
  }

  given Expr[Int] // error
  new Expr[Int] // error
  class Expr2 extends Expr[Int] // error

  given Type[Int] // error
  new Type[Int] // error
  class Type2 extends Type[Int] // error

}
