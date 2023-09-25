import scala.quoted.*

def readPathMacro[A: Type, B: Type](expr: Expr[Any])(using Quotes) =
  expr match
    case '{ foo($y) } => y: Expr[Int ?=> Int]

def foo(x: Int ?=> Int): Any = ???
