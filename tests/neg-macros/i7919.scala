import scala.quoted._

object Test {
  def staged[T](using qctx: QuoteContext) = {
    import qctx.tasty._
    given typeT as quoted.Type[T] // error
    val typeT2: quoted.Type[Int] = ???
    val tTypeTree = typeT2.unseal
    val tt = typeOf[T]
    '{ "in staged" }
  }

  given Expr[Int] // error
  new Expr[Int] // error
  class Expr2 extends Expr[Int] // error

  given Type // error
  new Type // error
  class Type2 extends Type // error

  given Type[Int] // error
  new Type[Int] // error
  class Type3 extends Type[Int] // error

}
