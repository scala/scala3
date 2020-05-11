import scala.quoted._

object Macro {
  inline def test[A, B]: Any =
    ${ impl[A, B] }

  def impl[A, B](using s: Scope)(using s.Type[A], s.Type[B]): s.Expr[Any] = {
    val t = '[Map[A, B]]
    '{
      new Object().asInstanceOf[$t]
      ???.asInstanceOf[$t]
    }
  }
}
