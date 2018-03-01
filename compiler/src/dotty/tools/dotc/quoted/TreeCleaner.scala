package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Constants._

/** Clean up quote artifacts from the tree to make it simpler to read.
 *  - Flattens block and remove blocks with not statements
 */
class TreeCleaner extends tpd.TreeMap {
  import tpd._

  override def transform(tree: Tree)(implicit ctx: Context): Tree = super.transform(tree) match {
    case Block(Nil, expr1) => expr1
    case Block(stats1, expr1) =>
      val flatStats = stats1.flatMap {
        case Block(stats2, expr2) => stats2 ::: expr2 :: Nil
        case Literal(Constant(())) => Nil
        case stat => stat :: Nil
      }
      expr1 match {
        case Block(stats3, expr3) => Block(flatStats ::: stats3, expr3)
        case expr3 => Block(flatStats, expr3)
      }
    case tree1 => tree1
  }
}
