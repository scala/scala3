import scala.quoted.{ QuoteContext, Expr, Type }

trait M[T] {
  def f: Any
}

inline def g[T: Type](em: Expr[M[T]])(using QuoteContext) = '{$em.f}
