import scala.quoted._
import scala.language.implicitConversions

object Macro {

  extension (strCtx: => StringContext) implicit inline def f3(args: =>Any*): String = ${FIntepolator.apply('strCtx, 'args)}

}

object FIntepolator {
  def apply(using s: Scope)(strCtxExpr: s.Expr[StringContext], argsExpr: s.Expr[Seq[Any]]): s.Expr[String] = {
    import s.tasty._
    error("there are no args", argsExpr.underlyingArgument.pos)
    '{ ($strCtxExpr).s($argsExpr: _*) }
  }

}
