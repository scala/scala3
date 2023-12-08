import scala.quoted.*

object MyMatcher {
  def unapply(expr: Expr[Any])(using Quotes): Option[Expr[Int]] = ???
}

object MyMatcher2 {
  def unapply(expr: Expr[Int])(using Quotes): Boolean = ???
}

def foo(x: Any): Unit = ???
def bar(x: Int): Int = ???

def oneLevel(expr: Expr[Any])(using Quotes): Expr[Int] = expr match
  case '{ foo(${MyMatcher(y@MyMatcher2())}) } => y

def twoLevel(expr: Expr[Any])(using Quotes): Expr[Int] = expr match
  case '{ foo(${MyMatcher('{ bar(${y@MyMatcher2()}).getClass}) }) } => y

def bindQuote(expr: Expr[Any])(using Quotes): Expr[Int] = expr match
  case '{ foo(${y@'{bar($_)}})} => y

def noop(expr: Expr[Any])(using Quotes): Expr[Int] = expr match
  case '{ bar(${ '{ $y }  }) } => y
