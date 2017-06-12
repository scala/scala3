package dotty.tools.dotc
package transform.localopt

import core.Contexts.Context
import ast.tpd.Tree

trait Optimisation {

  /** Run first to gather information on Trees (using mutation) */
  def visitor(implicit ctx: Context): Tree => Unit

  /** Does the actual Tree => Tree transformation, possibly using a different
   *  context from the one used in Optimisation.
   */
  def transformer(implicit ctx: Context): Tree => Tree

  def name: String = this.getClass.getSimpleName

  val NoVisitor: Tree => Unit = _ => ()
}
