package dotty.tools
package dotc
package transform

import core._
import Contexts._, Symbols._, Decorators._
import MegaPhase._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Types.MethodType
import dotty.tools.dotc.reporting.trace

class ErasedValCheck extends MiniPhase:
  import ast.tpd._

  override def phaseName: String = ErasedValCheck.name

  override def description: String = ErasedValCheck.description

  override def transformValDef(tree: ValDef)(using Context): Tree =
    if tree.symbol.is(Flags.Erased) then
      checkErasedRHS.transform(tree.rhs)
    tree

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
    val mt = tree.fun.tpe.widen.asInstanceOf[MethodType]
    if mt.hasErasedParams then
      tree.args.zip(mt.erasedParams).foreach((arg, isErased) => if isErased then checkErasedRHS.transform(arg))
    tree

end ErasedValCheck

object ErasedValCheck:
  val name: String = "erasedValCheck"
  val description: String = "check that erased vals conform to restricted forms"

private object checkErasedRHS extends ast.tpd.TreeMap {
  import ast.tpd._
  override def transform(tree: Tree)(using Context): Tree = {
    def checkRef(t: RefTree): Tree =
        val sym = t.denot.symbol
        if sym.is(Flags.Lazy) then
          report.error(i"Erased values cannot refer to a lazy val", t.srcPos)
        if sym.isRealMethod && !sym.isConstructor then
          report.error(i"Erased values cannot call non-constructor functions", t.srcPos)
        t
    def checkMemberDef(t: MemberDef) = t match
      case v: ValDef => transform(v.rhs)
      case _ => t

    tree match
      case id @ Ident(_) => checkRef(id)
      case m: MemberDef => checkMemberDef(m)
      case _ => super.transform(tree)
  }
}
