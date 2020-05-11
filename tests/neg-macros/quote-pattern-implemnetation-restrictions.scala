import scala.quoted._

def f(using s: Scope)(x: s.Expr[Any]) =
  x match {
    case '{ class Foo; () } => // error
    case '{ object Foo; () } => // error
    case '{ 1 match { case _ => () } } => // error
    case '{ try 1 finally () } => // error
    case '{ try 1 catch { case _ => 4 } } => // error
    case '{ Nil.map({ case x: Int => () }) } => // error
    case '{ def f: Int = return 2 } => // error
  }
