import scala.quoted.{ QuoteContext, Expr, Staged, Type }

trait M[T] {
  def f: Any
}

inline def g[T: Staged](em: Expr[M[T]])(using QuoteContext) = '{$em.f}
