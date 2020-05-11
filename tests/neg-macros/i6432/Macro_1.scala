
import scala.quoted._


object Macro {
  extension (inline sc: StringContext) inline def foo(args: String*): Unit = ${ impl('sc) }

  def impl(using s: Scope)(sc: s.Expr[StringContext]): s.Expr[Unit] = {
    import s.tasty._
    sc match {
      case '{ StringContext(${Varargs(parts)}: _*) } =>
        for (part @ Const(s) <- parts)
          error(s, part.pos)
    }
    '{}
  }
}
