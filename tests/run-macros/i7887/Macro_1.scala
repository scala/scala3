def myMacroImpl(a: quoted.Expr[_])(using qctx: quoted.QuoteContext) = {
  import qctx.reflect._
  def typed[A] = {
    implicit val t: quoted.Type[A] = a.asReflectTree.tpe.widen.asType.asInstanceOf[quoted.Type[A]]
    '{
      type T = A
      ${a.asExprOf[T]}
    }
  }

  typed
}


inline transparent def myMacro(a: => Any) = ${
  myMacroImpl('a)
}
