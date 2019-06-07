import scala.quoted._
import scala.tasty.Reflection
import scala.language.implicitConversions

object Macro {

  implicit inline def (strCtx: => StringContext) f3 (args: =>Any*): String = ${FIntepolator.apply('strCtx, 'args)}

}

object FIntepolator {
  def apply(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): Expr[String] = {
    import reflect._
    error("there are no args", argsExpr.unseal.underlyingArgument.pos)
    '{ ($strCtxExpr).s($argsExpr: _*) }
  }

}
