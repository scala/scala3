import scala.quoted._

object macros {
  inline def mcr(x: => Any): Any = ${mcrImpl('x)}

  class Foo // { def apply(str: String) = "100" }
  class Bar { def apply(str: String) = "100" }

  def mcrImpl(body: Expr[Any])(using ctx: Quotes): Expr[Any] = {
    body match {
      case '{($x: Foo)($bar: String)} => '{"Hello World"} // error
      case '{($x: Foo).apply($bar: String)} => '{"Hello World"} // error
      case '{($x: Bar)($bar: String)} => '{"Hello World"}
    }
  }
}
