case class Foo[A](a: A)

object Test {

  import scala.quoted._

  def impl[T](using s: Scope)(t: T)(using tt: s.Type[T]): s.Expr[Any] = {

    import s.tasty._
    import util._

    val foo = Type.of[Foo[String]]
    val symbol = foo.typeSymbol.field("a")
    val a = foo.select(symbol)
    assert(a <:< Type.of[String])

    '{???}
  }


  inline def apply[T](inline t: T) = ${ Test.impl('t )}

}
