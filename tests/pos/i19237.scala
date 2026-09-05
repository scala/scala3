package mono

trait C
object C {
  given C = new C {}
}
private[mono] object Focus:
  import internals.FocusImpl
  class MkFocus[From]:
    transparent inline def apply[To](inline f: C ?=> From => To): Any =
      ${ FocusImpl('f) }

package internals {
  import scala.quoted.*

  private[mono] object FocusImpl:
    def apply[From: Type, To: Type](f: Expr[C ?=> From => To])(using Quotes): Expr[Any] =
      f
}

/* did not actually fail to expand
object Test {
  def main() = {
    val mk = new Focus.MkFocus[String]
    mk.apply[Int] {
      _ => 42
    }
  }
}
*/
