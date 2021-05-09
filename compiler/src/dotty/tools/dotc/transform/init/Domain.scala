package dotty.tools.dotc
package transform
package init

import core._
import Symbols._

import ast.tpd._
import util.SourcePosition

trait Domain {
  /** Abstract values */
  type Value

  /** The bottom value */
  val bottom: Value

  /** Addresses to the abstract heap */
  type Addr

  /** Abstract object in the heap */
  type Objekt

  /** Abstract heap stores abstract objects */
  type Heap = Map[Addr, Objekt]

  /** Interpreter configuration
   *
   * The (abstract) interpreter can be seen as a push-down automaton that
   * transits between the configurations where the stack is the implicit call
   * stack of the meta-language.
   *
   * It's important that the configuration is finite for the analysis to
   * terminate.
   *
   * For soundness, we need to compute fixed point of the cache, which maps
   * configuration to evaluation result.
   *
   */
  type Config
  def makeConfig(expr: Tree, thisV: Value, heap: Heap): Config
}
