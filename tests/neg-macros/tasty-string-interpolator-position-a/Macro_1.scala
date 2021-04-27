import scala.quoted.*
import scala.language.implicitConversions

object Macro {

  extension (strCtx: => StringContext) implicit inline def f2(args: =>Any*): String = ${FIntepolator.apply('strCtx, 'args)}

}

object FIntepolator {

  def apply(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using Quotes) : Expr[String] = {
    import quotes.reflect.*
    report.error("there are no parts", strCtxExpr.asTerm.underlyingArgument.pos)
    '{ ($strCtxExpr).s($argsExpr*) }
  }

}
