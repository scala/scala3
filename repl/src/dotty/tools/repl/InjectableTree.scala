package dotty.tools
package repl

import dotc.core.Contexts.Context
import dotc.core.Decorators.PreNamedString
import dotc.ast.tpd.{ ModuleDef, Thicket, cpy }
import dotc.core.Flags.EmptyFlags
import dotc.core.Scopes
import dotc.ast.Trees.Template
import dotc.core.StdNames._

import results.Result

case class InjectableTree(obj: Thicket, nextId: Int)

object InjectableTree {
  def apply()(implicit ctx: Context) = new InjectableTree({
    val defn = ctx.definitions
    val sym = ctx.newCompleteModuleSymbol(
      ctx.owner, "ReplSession".toTermName,
      EmptyFlags, EmptyFlags, defn.AnyType :: Nil, Scopes.newScope
    )
    ModuleDef(sym, Nil)
  }, 0)

  def patch(tree: InjectableTree, res: TypedTrees)
           (implicit ctx: Context): Result[InjectableTree] = {

    val changedTrees = res.trees.map(t => t.changeOwner(t.symbol, tree.obj.symbol))
    val treeNames = changedTrees.map(_.symbol.show)

    println(tree.obj)

    new InjectableTree(cpy.Thicket(tree.obj)(Nil), tree.nextId)
  }
}
