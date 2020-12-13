
import scala.quoted._


object Macro {
  extension (inline sc: StringContext) inline def foo(args: String*): Unit = ${ impl('sc) }

  def impl(sc: Expr[StringContext])(using Quotes) : Expr[Unit] = {
    import quotes.reflect._
    sc match {
      case '{ StringContext(${Varargs(parts)}: _*) } =>
        for (part @ Const(s) <- parts)
          report.error(s, part.asTerm.pos)
    }
    '{}
  }
}
