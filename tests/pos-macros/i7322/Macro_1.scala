import scala.quoted._

trait M[T] {
  def f: Any
}

inline def g[T](using s: Scope)(em: s.Expr[M[T]])(using s.Type[T]) = '{$em.f}