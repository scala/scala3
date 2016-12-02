package dotty.tools
package dotc
package transform

import TreeTransforms.{ MiniPhaseTransform, TransformerInfo }
import core._
import Contexts.Context, Types._, Decorators._, Symbols._

class SpecializeExtendsFunction1 extends MiniPhaseTransform {
  import ast.tpd._

  val phaseName = "specializeExtendsFunction1"

  // types as abbreviated in scalac specialized class file names
  private[this] val typeAbbr = Map(
    "scala.Double"  -> "D",
    "scala.Float"   -> "F",
    "scala.Int"     -> "I",
    "scala.Long"    -> "J",
    "scala.Unit"    -> "V",
    "scala.Boolean" -> "Z"
  )

  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo) = {
    val parentAndTypes =
      tree.parents
      // map to a tuple of type and tree
      .map { t => (t.tpe, t) }
      // collect the parent that corresponds to Function1 and its type params
      .collect {
        case (RefinedType(RefinedType(parent, _, t1), _, r), function1)
          if parent.typeSymbol.showFullName == "scala.Function1" =>
            (function1, t1, r)
      }
      .headOption

    def replace(parent: Tree, in: List[Tree], withTree: Tree): List[Tree] =
      in.foldRight(List.empty[Tree]) { (t, trees) =>
        if (parent eq t) withTree :: trees
        else t :: trees
      }

    def specialized(t1: Type, r: Type, orig: Tree): Tree = {
      val t1Name = t1.typeSymbol.showFullName
      val rName  = r.typeSymbol.showFullName

      // get the required class via the abbreviations in `typeAbbr`, if they
      // don't both exist there, there is no specialization for the combination
      val requiredClass = (typeAbbr.get(t1Name), typeAbbr.get(rName)) match {
        case (Some(t1Abbr), Some(rAbbr)) =>
          ctx.requiredClassRef("scala.Function1$mc" + rAbbr + t1Abbr +  "$sp")
        case _ => orig.tpe
      }

      TypeTree(requiredClass)
    }

    parentAndTypes match {
      case Some((parent, t1, r)) =>
        val newParents = replace(parent, in = tree.parents, withTree = specialized(t1, r, parent))
        cpy.Template(tree)(parents = newParents)
      case _ => tree
    }
  }
}
