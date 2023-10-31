import scala.quoted.*

def m(using Quotes): Expr[Option[?]] =
  val s = 3
  type st = s.type
  '{ Some(${ Expr(s) }: st) } // error
