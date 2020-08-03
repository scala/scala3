case class Foo[A](a: A)

object Test {

  import scala.quoted._

  def impl[T](t: T)(using qctx: QuoteContext, tt: Staged[T]): Expr[Any] = {

    import qctx.tasty._
    import util._

    val foo = typeOf[Foo[String]]
    val symbol = foo.typeSymbol.field("a")
    val a = foo.select(symbol)
    assert(a <:< defn.StringType)

    '{???}
  }


  inline def apply[T](inline t: T) = ${ Test.impl('t )}

}
