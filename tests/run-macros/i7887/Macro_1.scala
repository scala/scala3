def myMacroImpl(a: quoted.Expr[_])(using qctx: quoted.QuoteContext) = {
  import qctx.tasty._
  def typed[A] = {
    implicit val t: quoted.Staged[A] = a.unseal.tpe.widen.seal.asInstanceOf[quoted.Staged[A]]
    '{
      type T = $t
      ${a.unseal.seal.cast[T]}
    }
  }

  typed
}


inline transparent def myMacro(a: => Any) = ${
  myMacroImpl('a)
}
