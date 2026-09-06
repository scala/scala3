package macrotest
import scala.quoted.*

def deepNestImpl(depth: Expr[Int])(using Quotes): Expr[Int] =
  val d = depth.valueOrAbort
  if d <= 0 then '{ 0 }
  else '{ 1 + ${ deepNestImpl(Expr(d - 1)) } }
