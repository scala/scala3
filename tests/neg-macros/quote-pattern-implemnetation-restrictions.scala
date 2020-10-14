import scala.quoted._

def f(x: Expr[Any])(using QuoteContext) =
  x match {
    case '{ class Foo; () } => // error
    case '{ object Foo; () } => // error
    case '{ 1 match { case _ => () } } => // error // error
    case '{ try 1 finally () } => // error // error
    case '{ try 1 catch { case _ => 4 } } => // error // error
    case '{ Nil.map({ case x: Int => () }) } => // error
    case '{ def f: Int = return 2 } => // error
  }
