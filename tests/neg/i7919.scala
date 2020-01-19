import scala.quoted._

object Test {
  def staged[T] with (qctx: QuoteContext) = {
    import qctx.tasty.{_, given _}
    given typeT as quoted.Type[T] // error
    val tTypeTree = typeT.unseal
    val tt = typeOf[T]
    '{ "in staged" }
  }

  given Expr[Int] // error
  new Expr[Int] // error
  class Expr2 extends Expr[Int] // error

  given Type[Int] // error
  new Type[Int] // error
  class Type2 extends Type[Int] // error

}
