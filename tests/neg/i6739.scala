import scala.quoted.{_, given}

inline def assert(expr: => Boolean): Unit =
  ${ assertImpl('expr) } // error: Macro cannot be implemented with an `inline` method

inline def assertImpl(expr: Expr[Boolean]): Expr[Unit] = '{ println("Hello World") }
