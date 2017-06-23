package dotty.tools.dotc
package transform.localopt

import core.Contexts.Context
import ast.tpd.Tree

trait Optimisation {

  /** Gathers information on trees (using mutation), to be run first. */
  def visitor(implicit ctx: Context): Tree => Unit

  /** Does the actual Tree => Tree transformation. */
  def transformer(implicit ctx: Context): Tree => Tree

  /** Clears all the state of this optimisation, to be run last. */
  def clear(): Unit

  def name: String = this.getClass.getSimpleName

  val NoVisitor: Tree => Unit = _ => ()
}
