import scala.quoted.{ Quotes, Expr }

trait Foo

inline def g(em: Expr[Foo])(using Quotes) = '{$em}
