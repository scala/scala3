import quoted._
import quoted.unsafe._
object Main {

  def myMacroImpl(body: Expr[_])(using qctx: QuoteContext) : Expr[_] = {
    import qctx.reflect._
    val bodyTerm = UnsafeExpr.underlyingArgument(body).unseal
    val showed = bodyTerm.show
    '{
      println(${Expr(showed)})
      ${bodyTerm.seal}
    }
  }

  transparent inline def myMacro(body: => Any): Any = ${
    myMacroImpl('body)
  }
}
