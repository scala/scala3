package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.modifiers

object Modifiers {

  def apply(tree: tpd.MemberDef)(implicit ctx: Context): List[modifiers.Modifier] = {
    QualifiedModifier(tree).toList :::
    tree.symbol.annotations.map(AnnotationModifier(_)) :::
    tree.rawMods.mods.map(Modifier(_))
  }

}
