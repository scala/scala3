import scala.quoted._

trait M {
  def f: Any
}

inline def g(using s: Scope)(em: s.Expr[M]) = '{$em.f}