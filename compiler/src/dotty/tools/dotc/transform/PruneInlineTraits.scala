package dotty.tools.dotc
package transform

import core._
import Contexts._
import DenotTransformers.SymTransformer
import Flags._
import SymDenotations._
import Symbols._
import MegaPhase.MiniPhase
import ast.tpd
import dotty.tools.dotc.core.StdNames.str

class PruneInlineTraits extends MiniPhase with SymTransformer { thisTransform =>
  import tpd._
  import PruneInlineTraits._

  override def phaseName: String = PruneInlineTraits.name

  override def description: String = PruneInlineTraits.description

  override def transformSym(sym: SymDenotation)(using Context): SymDenotation =
    if isEraseable(sym) then sym.copySymDenotation(initFlags = sym.flags | Deferred)
    else if sym.isInlineTrait then
      val clsInfo = sym.asClass.classInfo
      val clsInfo2 = clsInfo.derivedClassInfo(decls = 
          clsInfo.decls.filteredScope(!isDeletable(_))
      )
      sym.copySymDenotation(initFlags = sym.flags | PureInterface | NoInits, info = clsInfo2)
    else sym

  override def transformTemplate(tree: Template)(using Context): Tree = 
    cpy.Template(tree)(body = tree.body.flatMap({
      case stmt: ValDef if isEraseable(stmt.symbol) => Some(cpy.ValDef(stmt)(rhs = EmptyTree))
      case stmt: DefDef if isEraseable(stmt.symbol) => Some(cpy.DefDef(stmt)(rhs = EmptyTree))
      case stmt: (ValDef | DefDef) if isDeletable(stmt.symbol) => None
      case stmt => Some(stmt)
    }))

  private def isEraseable(sym: SymDenotation)(using Context): Boolean =
    !sym.isType
    && !sym.isConstructor
    && !sym.is(Param)
    && !sym.is(ParamAccessor)
    && !sym.is(Local)
    && !sym.isLocalDummy
    && sym.exists
    && sym.owner.isInlineTrait

  private def isDeletable(sym: SymDenotation)(using Context): Boolean = 
    !sym.isType
    && sym.owner.isInlineTrait
    && (sym.is(Local) || sym.is(Inline))
    && !sym.is(Param)
    && !sym.is(ParamAccessor)
}

object PruneInlineTraits {
  import tpd._

  val name: String = "pruneInlineTraits"
  val description: String = "drop rhs definitions in inline traits"
}
