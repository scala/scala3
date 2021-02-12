import scala.quoted.*

inline def assert(expr: => Boolean): Unit =
  ${ assertImpl('expr) } // error: Macro cannot be implemented with an `inline` method

inline def assertImpl(expr: Expr[Boolean])(using Quotes): Expr[Unit] = '{ println("Hello World") }
