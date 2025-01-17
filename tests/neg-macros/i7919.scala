import scala.quoted.*

object Test {
  def staged[T](using Quotes) = {
    import quotes.reflect.*
    given typeT: Type[T] {} // error
    val tt = TypeRepr.of[T]
    '{ "in staged" }
  }

  given Expr[Int] {} // error
  new Expr[Int] // error
  class Expr2 extends Expr[Int] // error

  given Type[Int] {} // error
  new Type[Int] // error
  class Type2 extends Type[Int] // error

}
