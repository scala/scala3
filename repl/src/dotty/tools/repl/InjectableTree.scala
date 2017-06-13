package dotty.tools
package repl

import dotc.core.Contexts.Context
import dotc.ast.tpd._

import results._

case class InjectableTree(obj: TypeDef, nextId: Int)

object InjectableTree {
  def apply()(implicit ctx: Context) = new InjectableTree({
    ???
  }, 0)

  def patch(tree: InjectableTree, res: TypedTrees)
           (implicit ctx: Context): Result[InjectableTree] =
    ???
}
