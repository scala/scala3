import scala.quoted._

object Macro {
  inline def test[A, B]: Any =
    ${ impl[A, B] }

  def impl[A : Type, B : Type](using QuoteContext): Expr[Any] = {
    val t = Type[Map[A, B]]
    '{
      new Object().asInstanceOf[t.T]
      ???.asInstanceOf[t.T]
    }
  }
}
