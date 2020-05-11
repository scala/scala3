import scala.quoted._

inline def assert(expr: => Boolean): Unit =
  ${ assertImpl('expr) } // error: Macro cannot be implemented with an `inline` method

inline def assertImpl(using s: Scope)(expr: s.Expr[Boolean]): s.Expr[Unit] = '{ println("Hello World") }
