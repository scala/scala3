package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Denotations._
import core.SymDenotations._
import core.Contexts._
import core.Symbols._
import core.Types._
import core.Constants._
import core.StdNames._
import core.transform.Erasure.isUnboundedGeneric
import typer.ErrorReporting._
import ast.Trees._

/** This transform eliminates patterns. Right now it's a dummy.
 *  Awaiting the real pattern matcher.
 */
class TreeChecker {
  import ast.tpd._

  def check(ctx: Context) = {
    println(s"checking ${ctx.compilationUnit} after phase ${ctx.phase.prev}")
    Checker.transform(ctx.compilationUnit.tpdTree)(ctx)
  }

  object Checker extends TreeMap {
    override def transform(tree: Tree)(implicit ctx: Context) = {
      println(i"checking $tree")
      assert(tree.isEmpty || tree.hasType, tree.show)
      super.transform(tree)
    }
  }
}

object TreeChecker extends TreeChecker