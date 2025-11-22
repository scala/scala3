import caps.{cap, Mutable}
import caps.unsafe.untrackedCaptures

class Test extends Mutable:
  var ctxStack: Array[FreshCtx^] = new Array(10)

  class FreshCtx(level: Int) extends Mutable:
    this: FreshCtx^ =>
    def detached: Boolean =
      val c: FreshCtx^{cap.rd} = ctxStack(level)
      (c eq this)
    def detached2 =
      ctxStack(level) eq this
