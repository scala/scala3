package dotty.tools.dotc.interpreter

import dotty.tools.dotc.ast.tpd

class RawExpr(val tree: tpd.Tree) extends quoted.Expr[Any] with RawQuoted {
  override def toString: String = s"RawExpr(${tree.toString})"
}
