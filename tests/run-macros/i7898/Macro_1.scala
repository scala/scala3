import quoted._
object Main {

  def myMacroImpl(body: Expr[_]) with (qctx: QuoteContext) : Expr[_] = {
    import qctx.tasty.{_, given _}
    val bodyTerm = body.underlyingArgument.unseal
    val showed = bodyTerm.show
    '{
      println(${Expr(showed)})
      ${bodyTerm.seal}
    }
  }

  inline def myMacro(body: => Any) <: Any = ${
    myMacroImpl('body)
  }
}
