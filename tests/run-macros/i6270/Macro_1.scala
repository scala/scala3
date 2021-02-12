import scala.quoted.*

object api {
  extension (inline x: String) inline def reflect : String =
    ${ reflImpl('x) }

  private def reflImpl(x: Expr[String])(using Quotes) : Expr[String] = {
    import quotes.reflect.*
    Expr(x.show)
  }

  extension (x: => String) inline def reflectColor : String =
    ${ reflImplColor('x) }

  private def reflImplColor(x: Expr[String])(using Quotes) : Expr[String] = {
    import quotes.reflect.*
    Expr(x.asTerm.show(using Printer.TreeAnsiCode))
  }
}
