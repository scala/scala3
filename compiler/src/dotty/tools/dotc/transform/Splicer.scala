package dotty.tools.dotc
package transform

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.interpreter._
import dotty.tools.dotc.quoted.PickledQuotes

import scala.quoted

/** Utility class to slice quoted expressions */
object Splicer {
  import tpd._

  /** Splice the Tree for a Quoted expression. `~'(xyz)` becomes `xyz`
   *  and for `~xyz` the tree of `xyz` is interpreted for which the
   *  resulting expression is return as a `Tree`
   */
  def splice(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case Apply(quote, quoted :: Nil) if quote.symbol == defn.quoteMethod => quoted
    case TypeApply(quote, quoted :: Nil) if quote.symbol == defn.typeQuoteMethod => quoted
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
