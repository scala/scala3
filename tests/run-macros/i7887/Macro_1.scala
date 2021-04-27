def myMacroImpl(a: quoted.Expr[_])(using qctx: quoted.Quotes) = {
  import scala.quoted.quotes.reflect.*
  def typed[A] = {
    implicit val t: quoted.Type[A] = a.asTerm.tpe.widen.asType.asInstanceOf[quoted.Type[A]]
    '{
      type T = A
      ${a.asTerm.asExprOf[T]}
    }
  }

  typed
}


inline transparent def myMacro(a: => Any) = ${
  myMacroImpl('a)
}
