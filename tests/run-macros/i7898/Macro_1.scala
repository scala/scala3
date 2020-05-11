import quoted._

object Main {

  def myMacroImpl(using s: Scope)(body: s.Expr[Any]): s.Expr[Any] = {
    import s.tasty._
    val bodyTerm = body.underlyingArgument
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
