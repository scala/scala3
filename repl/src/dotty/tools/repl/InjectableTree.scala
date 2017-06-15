package dotty.tools
package repl

import dotc.core.Contexts.Context
import dotc.core.Decorators.PreNamedString
import dotc.ast.tpd.{ ModuleDef, Thicket, cpy }
import dotc.core.Flags.EmptyFlags
import dotc.core.Scopes
import dotc.ast.Trees._
import dotc.ast.tpd
import dotc.core.StdNames._

import results.Result

case class InjectableTree(obj: Thicket, nextId: Int)

object InjectableTree {
  /** Create the initial InjectableTree - after being called once, should never
   *  be called again
   */
  def apply()(implicit ctx: Context) = new InjectableTree({
    val defn = ctx.definitions
    val sym = ctx.newCompleteModuleSymbol(
      defn.RootPackage, "ReplSession".toTermName,
      EmptyFlags, EmptyFlags, defn.AnyType :: Nil, Scopes.newScope
    )
    ModuleDef(sym, Nil)
  }, 0)

  def patch(tree: InjectableTree, res: TypedTrees)
           (implicit ctx: Context): Result[InjectableTree] = {

    val changedTrees =
      res.trees.map(t => t.changeOwner(t.symbol, tree.obj.trees(1).symbol)).toList

    val statsReplacer = new tpd.TreeMap {
      override def transformStats(trees: List[tpd.Tree])(implicit ctx: Context) =
        changedTrees
    }

    new InjectableTree(
      statsReplacer.transform(tree.obj).asInstanceOf[tpd.Thicket],
      tree.nextId
    )
  }
}
