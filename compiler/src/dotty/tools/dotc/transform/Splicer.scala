package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.quoted._
import dotty.tools.dotc.interpreter._

/** Utility class to slice quoted expressions */
object Splicer {
  import tpd._

  /** Splice the Tree for a Quoted expression. `~'(xyz)` becomes `xyz`
   *  and for `~xyz` the tree of `xyz` is interpreted for which the
   *  resulting expression is return as a `Tree`
   */
  def splice(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case Quoted(quotedTree) => quotedTree
    case tree: RefTree => reflectiveSplice(tree)
    case tree: Apply => reflectiveSplice(tree)
    case tree: Inlined => reflectiveSplice(tree)
  }

  /** Splice the Tree for a Quoted expression which is constructed via a reflective call to the given method */
  private def reflectiveSplice(tree: Tree)(implicit ctx: Context): Tree = {
    val interpreter = new Interpreter
    interpreter.interpretTree[quoted.Expr[_]](tree).map(PickledQuotes.quotedToTree(_)).getOrElse(tree)
  }

}
