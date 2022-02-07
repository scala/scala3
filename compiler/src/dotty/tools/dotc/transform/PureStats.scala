package dotty.tools.dotc
package transform

import ast.{Trees, tpd}
import core._, core.Decorators._
import MegaPhase._
import Types._, Contexts._, Flags._, DenotTransformers._
import Symbols._, StdNames._, Trees._

object PureStats {
  val name: String = "pureStats"
  val description: String = "remove pure statements in blocks"
}

/** Remove pure statements in blocks */
class PureStats extends MiniPhase {

  import tpd._

  override def phaseName: String = PureStats.name

  override def description: String = PureStats.description

  override def runsAfter: Set[String] = Set(Erasure.name)

  override def transformBlock(tree: Block)(using Context): Tree =
    val stats = tree.stats.mapConserve {
      case Typed(Block(stats, expr), _) if isPureExpr(expr) => Thicket(stats)
      case stat if !stat.symbol.isConstructor && isPureExpr(stat) => EmptyTree
      case stat => stat
    }
    if stats eq tree.stats then tree
    else cpy.Block(tree)(Trees.flatten(stats), tree.expr)

}
