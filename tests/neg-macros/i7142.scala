package macros
import scala.quoted._
import scala.util.control.NonLocalReturns._

def oops(using s: Scope): s.Expr[Int] =
  returning('{ { (x: Int) => ${ throwReturn('x) }} apply 0 }) // error
