import scala.quoted.reflect

def myMacroImpl(a: quoted.Expr[_])(using qctx: quoted.QuoteContext) = {
  import reflect._
  def typed[A] = {
    implicit val t: quoted.Type[A] = Term.of(a).tpe.widen.asType.asInstanceOf[quoted.Type[A]]
    '{
      type T = A
      ${Term.of(a).asExprOf[T]}
    }
  }

  typed
}


inline transparent def myMacro(a: => Any) = ${
  myMacroImpl('a)
}
