package mono

trait C
private[mono] object Focus:
  import internals.FocusImpl
  class MkFocus[From]:
    transparent inline def apply[To](inline f: C ?=> From => To): Any =
      ${ FocusImpl('f) }

package internals {
  import scala.quoted.*

  private[mono] object FocusImpl:
    def apply[From: Type, To: Type](f: Expr[C ?=> From => To])(using Quotes): Expr[Any] =
      ???
}
