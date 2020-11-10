def myMacroImpl(a: quoted.Expr[_])(using qctx: quoted.QuoteContext) = {
  import qctx.reflect._
  def typed[A] = {
    implicit val t: quoted.Type[A] = a.unseal.tpe.widen.asType.asInstanceOf[quoted.Type[A]]
    '{
      type T = A
      ${a.unseal.asExprOf[T]}
    }
  }

  typed
}


inline transparent def myMacro(a: => Any) = ${
  myMacroImpl('a)
}
