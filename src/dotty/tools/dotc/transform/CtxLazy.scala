package dotty.tools.dotc
package util
import core.Contexts.Context

class CtxLazy[T](expr: Context => T) {
  private var myValue: T = _
  private var forced = false
  def apply()(implicit ctx: Context): T = {
    if (!forced) myValue = expr(ctx)
    myValue
  }
}