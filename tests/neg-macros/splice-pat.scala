import scala.quoted.*

object MyMatcher {
  def unapply(expr: Expr[Any])(using Quotes): Option[Expr[Int]] = ???
}

def foo(x: Any): Unit = ???

def bar(): Expr[Any] = ???

def f(expr: Expr[Any])(using Quotes): Expr[Int] = expr match
  case '{ foo(${  // error: pattern expected
    import scala.Int
    bar()
    })} => ???  // error
