//> using options -explain

import scala.quoted.{Expr, Quotes}

case class Thing[T]()

def foo[T](using Quotes): Expr[Thing[T]] = '{ Thing[T]() } // error
