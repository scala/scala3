import scala.quoted._

trait Foo

inline def g(using s: Scope)(em: s.Expr[Foo]) = '{$em}
