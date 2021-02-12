case class Foo[A](a: A)

object Test {

  import scala.quoted.*

  def impl[T](t: T)(using Quotes, Type[T]): Expr[Any] = {

    import quotes.reflect.*
    import util.*

    val foo = TypeRepr.of[Foo[String]]
    val symbol = foo.typeSymbol.memberField("a")
    val a = foo.select(symbol)
    assert(a <:< TypeRepr.of[String])

    '{???}
  }


  inline def apply[T](inline t: T) = ${ Test.impl('t )}

}
