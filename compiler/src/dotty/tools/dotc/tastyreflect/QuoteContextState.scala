package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.util.Property

object QuoteContextState {

  val NextIndex = new Property.Key[scala.runtime.IntRef]

  def init(ctx: Context): Context =
    ctx.fresh.setProperty(NextIndex, new scala.runtime.IntRef(0))

  def nextIndex()(given ctx: Context): Int = {
    val ref = ctx.property(NextIndex).get
    val ret = ref.elem
    ref.elem = ret + 1
    ret
  }
}

