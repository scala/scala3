package dotty.tools.dotc
package transform

import core.Contexts._

/** Utility class for lazy values whose evaluation depends on a context.
 *  This should be used whenever the evaluation of a lazy expression
 *  depends on some context, but the value can be re-used afterwards
 *  with a different context.
 *
 *  A typical use case is a lazy val in a phase object which exists once per root context where
 *  the expression initializing the lazy val depends only on the root context, but not any changes afterwards.
 */
class CtxLazy[T](expr: Context ?=> T) {
  private var myValue: T = _
  private var forced = false
  def apply()(using Context): T = {
    if (!forced) {
      myValue = expr
      forced = true
    }
    myValue
  }
}
