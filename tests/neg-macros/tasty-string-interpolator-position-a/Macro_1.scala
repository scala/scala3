import scala.quoted._
import scala.language.implicitConversions

object Macro {

  extension (strCtx: => StringContext) implicit inline def f2(args: =>Any*): String = ${FIntepolator.apply('strCtx, 'args)}

}

object FIntepolator {

  def apply(using s: Scope)(strCtxExpr: s.Expr[StringContext], argsExpr: s.Expr[Seq[Any]]): s.Expr[String] = {
    import s.tasty._
    error("there are no parts", strCtxExpr.underlyingArgument.pos)
    '{ ($strCtxExpr).s($argsExpr: _*) }
  }

}
