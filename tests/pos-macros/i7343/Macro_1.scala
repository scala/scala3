import scala.quoted.{ Quotes, Expr }

trait M {
  def f: Any
}

inline def g(em: Expr[M])(using Quotes) = '{$em.f}