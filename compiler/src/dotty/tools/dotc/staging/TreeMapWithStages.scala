package dotty.tools.dotc
package staging

import dotty.tools.dotc.ast.{TreeMapWithImplicits, tpd}
import dotty.tools.dotc.config.Printers.staging
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.staging.StagingLevel.*
import dotty.tools.dotc.util.Lst

import scala.collection.mutable

/** TreeMap that keeps track of staging levels using StagingLevel. */
abstract class TreeMapWithStages extends TreeMapWithImplicits {
  import tpd.*

  override def transform(tree: Tree)(using Context): Tree =
    if (tree.source != ctx.source && tree.source.exists)
      transform(tree)(using ctx.withSource(tree.source))
    else reporting.trace(i"TreeMapWithStages.transform $tree at $level", staging, show = true) {
      tree match {
        case Block(stats, _) =>
          val defSyms = stats.collect { case defTree: DefTree => defTree.symbol }
          super.transform(tree)(using symbolsInCurrentLevel(defSyms))

        case CaseDef(pat, guard, body) =>
          super.transform(tree)(using symbolsInCurrentLevel(tpd.patVars(pat)))

        case (_:Import | _:Export) =>
          tree

        case _: Template =>
          val decls = tree.symbol.owner.info.decls.toList
          super.transform(tree)(using symbolsInCurrentLevel(decls))

        case LambdaTypeTree(tparams, body) =>
          super.transform(tree)(using symbolsInCurrentLevel(tparams.mapToList(_.symbol)))

        case tree: DefTree =>
          val paramSyms = tree match
            case tree: DefDef => tree.paramss.flattenLst.map(_.symbol).toList
            case _ => Nil
          super.transform(tree)(using symbolsInCurrentLevel(tree.symbol :: paramSyms))

        case _ =>
          super.transform(tree)
      }
    }
}
