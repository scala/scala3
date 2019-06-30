import scala.quoted._

object Macro {
  inline def test[A, B]: Any =
    ${ impl[A, B] }

  def impl[A : Type, B : Type] given QuoteContext: Expr[Any] = {
    val t = '[Map[A, B]]
    '{
      new Object().asInstanceOf[$t]
      ???.asInstanceOf[$t]
    }
  }
}
