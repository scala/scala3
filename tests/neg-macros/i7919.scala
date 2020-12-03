import scala.quoted._

object Test {
  def staged[T](using Quotes) = {
    import quotes.reflect._
    given typeT: Type[T] with {} // error
    val tt = TypeRepr.of[T]
    '{ "in staged" }
  }

  given Expr[Int] with {} // error
  new Expr[Int] // error
  class Expr2 extends Expr[Int] // error

  given Type[Int] with {} // error
  new Type[Int] // error
  class Type2 extends Type[Int] // error

}
