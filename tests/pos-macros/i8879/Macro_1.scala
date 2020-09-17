case class Foo[A](a: A)

object Test {

  import scala.quoted._

  def impl[T](t: T)(using qctx: QuoteContext, tt: Type[T]): Expr[Any] = {

    import qctx.tasty._
    import util._

    val foo = Type.of[Foo[String]]
    val symbol = foo.typeSymbol.field("a")
    val a = foo.select(symbol)
    assert(a <:< Type.of[String])

    '{???}
  }


  inline def apply[T](inline t: T) = ${ Test.impl('t )}

}
