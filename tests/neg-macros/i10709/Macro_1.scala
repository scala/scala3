import scala.quoted.*
import scala.compiletime.summonInline

object Invalid {
  inline def apply[A, B]: Any = ${ invalidImpl[A, B] }

  def invalidImpl[A, B](using qctx: Quotes, tpeA: Type[A], tpeB: Type[B]): Expr[Any] = {
    '{summonInline[B <:< A]}
  }
}
