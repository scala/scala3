import scala.quoted.{ Quotes, Expr, Type }

trait M[T] {
  def f: Any
}

inline def g[T: Type](em: Expr[M[T]])(using Quotes) = '{$em.f}