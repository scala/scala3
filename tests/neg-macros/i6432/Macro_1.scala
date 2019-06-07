
import scala.quoted._
import scala.quoted.autolift._
import scala.quoted.matching._

object Macro {
  inline def (sc: => StringContext) foo (args: String*): Unit = ${ impl('sc) }

  def impl(sc: Expr[StringContext])(implicit reflect: tasty.Reflection): Expr[Unit] = {
    import reflect._
    sc match {
      case '{ StringContext(${ExprSeq(parts)}: _*) } =>
        for (part @ Const(s) <- parts)
          error(s, part.unseal.pos)
    }
    '{}
  }
}
