import scala.quoted._

inline def assert(expr: => Boolean): Unit =
  ${ assertImpl('expr) } // error: Macro cannot be implemented with an `inline` method

inline def assertImpl(expr: Expr[Boolean]): Expr[Unit] = '{ println("Hello World") }
