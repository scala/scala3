import scala.quoted.{ QuoteContext, Expr }

trait M {
  def f: Any
}

inline def g(em: Expr[M])(using QuoteContext) = '{$em.f}