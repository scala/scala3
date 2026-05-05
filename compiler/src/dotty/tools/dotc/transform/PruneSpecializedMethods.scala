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

class PruneSpecializedMethods extends MiniPhase with SymTransformer { thisTransform =>
  import tpd._
  import PruneSpecializedMethods._

  override def phaseName: String = PruneSpecializedMethods.name

  override def description: String = PruneSpecializedMethods.description

  override def transformSym(sym: SymDenotation)(using Context): SymDenotation =
    if sym.isClass && !sym.is(Package) then
      val clsInfo = sym.asClass.classInfo
      val clsInfo2 = clsInfo.derivedClassInfo(decls = 
          clsInfo.decls.filteredScope(!isDeletable(_))
      )
      sym.copySymDenotation(info = clsInfo2)
    else sym

  override def transformTemplate(tree: Template)(using Context): Tree = 
    cpy.Template(tree)(body = tree.body.flatMap({
      case stmt: DefDef if isDeletable(stmt.symbol) => None
      case stmt => Some(stmt)
    }))

  private def isDeletable(sym: SymDenotation)(using Context): Boolean = sym.isSpecializedMethod
}

object PruneSpecializedMethods {
  import tpd._

  val name: String = "pruneSpecializedMethods"
  val description: String = "drop specialized methods which have already been inlined; we can't wait until erasure because they can be broken by pruneInlineTraits removing members from the specialized traits they instantiate"
}
