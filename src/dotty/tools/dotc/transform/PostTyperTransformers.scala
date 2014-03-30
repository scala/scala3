package dotty.tools.dotc.transform

import dotty.tools.dotc.core._
import Symbols._
import scala.Some
import dotty.tools.dotc.transform.TreeTransforms.{NXTransformations, TransformerInfo, TreeTransform, TreeTransformer}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import scala.collection.mutable
import dotty.tools.dotc.core.Names.Name
import NameOps._

object PostTyperTransformers {

  import tpd._


  /** A trait that's assumed by the transformers that run right after typer.
    * Ensures that trees are normalized when seen by other transforms. This means:
    * (1) All module class definitions appear after their companion class definitions
    * (2) There are no import clauses or named arguments
    * (3) All trees designating types are instances of TypeTree
    */
  abstract class PostTyperTransformer extends TreeTransformer {

    /** Reorder statements so that module classes always come after their companion classes, add missing companion classes */
    def reorder(stats: List[Tree])(implicit ctx: Context, info: TransformerInfo): List[Tree] = {
      val moduleClassDefs = mutable.Map[Name, Tree]()
      def reorder0(stats: List[Tree]): List[Tree] = {
        stats match {
          case (stat: TypeDef) :: stats1 if stat.symbol.isClass =>
            if (stat.symbol is Flags.Module) {
              moduleClassDefs += (stat.name -> stat)
              val stats1r = reorder0(stats1)
              if (moduleClassDefs contains stat.name) stat :: stats1r else stats1r
            }
            else {
              val mclsName = stat.name.moduleClassName
              moduleClassDefs remove mclsName match {
                case Some(mcdef) => stat :: mcdef :: reorder0(stats1)
                case None => stat :: reorder0(stats1)
              }
            }
          case stat :: stats1 => stat :: reorder0(stats1)
          case Nil => Nil
        }
      }
      reorder0(stats)
    }

    override def transformStats(trees: List[tpd.Tree], exprOwner: Symbol, info: TransformerInfo, current: Int)(implicit ctx: Context): List[tpd.Tree] =
      super.transformStats(reorder(trees)(ctx, info), exprOwner, info, current)

    override def transform(tree: tpd.Tree, info: TransformerInfo, cur: Int)(implicit ctx: Context): tpd.Tree = tree match {
      case tree: Import => EmptyTree
      case tree: NamedArg => super.transform(tree.arg, info, cur)
      case tree: TypeTree => super.transform(tree, info, cur)
      case tree => super.transform(if (tree.isType) TypeTree(tree.tpe) else tree, info, cur)
    }
  }

}