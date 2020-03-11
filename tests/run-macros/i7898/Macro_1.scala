import quoted._
import quoted.unsafe._
object Main {

  def myMacroImpl(body: Expr[_])(using qctx: QuoteContext) : Expr[_] = {
    import qctx.tasty._
    val bodyTerm = UnsafeExpr.underlyingArgument(body).unseal
    val showed = bodyTerm.show
    '{
      println(${Lifted(showed)})
      ${bodyTerm.seal}
    }
  }

  inline def myMacro(body: => Any) <: Any = ${
    myMacroImpl('body)
  }
}
