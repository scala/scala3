package dotty.tools
package dotc
package transform

import TreeTransforms.{ MiniPhaseTransform, TransformerInfo }
import core._
import Contexts.Context, Types._, Decorators._, Symbols._

class SpecializeExtendsFunction1 extends MiniPhaseTransform {
  import ast.tpd._

  val phaseName = "specializeExtendsFunction1"

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

      val requiredClass = (t1Name, rName) match {
        case ("scala.Int", "scala.Int") =>
          ctx.requiredClassRef("scala.Function1$mcII$sp")
        case ("scala.Char", "scala.Int") =>
          ctx.requiredClassRef("scala.Function1$mcCI$sp")
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
