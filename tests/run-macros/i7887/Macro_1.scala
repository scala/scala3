def myMacroImpl(a: quoted.Expr[_])(using qctx: quoted.Quotes) = {
  import scala.quoted.quotes.reflect._
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
