package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

/** Normalizes names of type definitions to avoid OS-dependent name clashes/shadowing */

class NormalizePackageDefs extends MiniPhase {
  import tpd._

  override def phaseName = "normalizePackageDefs"

  override def transformPackageDef(tree: PackageDef)(implicit ctx: Context) : Tree = {

    val typeList = tree.stats.filter(s => s.denot.isType)
    val normalizedDefinitionList = typeList.map(s => s.denot.toString.toLowerCase())
    if (normalizedDefinitionList.distinct.size < typeList.size) ctx.error(i"package ${tree.denot.name} contains definitions with duplicate names, in a case-insensitive manner", tree.pos)

    tree
  }
}
