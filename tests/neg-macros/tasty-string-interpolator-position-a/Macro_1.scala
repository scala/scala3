import scala.quoted._
import scala.tasty.Reflection
import scala.language.implicitConversions

object Macro {

  implicit inline def (strCtx: => StringContext) f2 (args: =>Any*): String = ${FIntepolator.apply('strCtx, 'args)}

}

object FIntepolator {

  def apply(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]) given (qctx: QuoteContext): Expr[String] = {
    import qctx.tasty._
    error("there are no parts", strCtxExpr.unseal.underlyingArgument.pos)
    '{ ($strCtxExpr).s($argsExpr: _*) }
  }

}
