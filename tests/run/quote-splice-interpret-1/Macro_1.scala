import dotty.tools.dotc.ast.Trees.Import

import scala.quoted._

object Macros {
  sealed trait Nat
  case object Z extends Nat
  case class S[N <: Nat]() extends Nat

  transparent def isZero(transparent n: Int): Boolean = ~{
    if (n == 0) '(true)
    else '(false)
  }

}
