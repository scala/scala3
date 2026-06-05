import scala.quoted._
def expr(expr: Expr[Any])(using Quotes) =
  expr match { case '{ type t : List; $x: t } => ??? } // error
