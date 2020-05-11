import scala.quoted._
import scala.quoted.show.SyntaxHighlight.ANSI

object api {
  extension (inline x: String) inline def reflect : String =
    ${ reflImpl('x) }

  private def reflImpl(using s: Scope)(x: s.Expr[String]): s.Expr[String] =
    Expr(x.show)

  extension (x: => String) inline def reflectColor : String =
    ${ reflImplColor('x) }

  private def reflImplColor(using s: Scope)(x: s.Expr[String]): s.Expr[String] =
    Expr(x.showWith(ANSI))
}
