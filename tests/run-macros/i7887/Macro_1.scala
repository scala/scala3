import scala.quoted._

def myMacroImpl(using s: Scope)(a: s.Expr[Any]) = {
  import s.tasty._
  def typed[A] = {
    implicit val t: s.Type[A] = a.tpe.widen.seal.get.asInstanceOf[s.Type[A]]
    '{
      type T = $t
      ${a.seal.cast[T]}
    }
  }

  typed
}


inline transparent def myMacro(a: => Any) = ${
  myMacroImpl('a)
}
