def myMacroImpl(a: quoted.Expr[_])(using qctx: quoted.QuoteContext) = {
  import qctx.tasty._
  def typed[A] = {
    implicit val t: quoted.Type[A] = a.unseal.tpe.widen.seal.asInstanceOf[quoted.Type[A]]
    '{
      type T = t.T
      ${a.unseal.seal.cast[T]}
    }
  }

  typed
}


inline transparent def myMacro(a: => Any) = ${
  myMacroImpl('a)
}
