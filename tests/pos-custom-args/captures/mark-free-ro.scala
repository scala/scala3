import caps.{any, Stateful}
import caps.unsafe.untrackedCaptures

class Test extends Stateful:
  var ctxStack: Array[FreshCtx^] = new Array(10)

  class FreshCtx(level: Int) extends Stateful:// uses Test.this: !!! uncomment --> error
    this: FreshCtx^ =>
    def detached: Boolean =
      val c: FreshCtx^{any.rd} = ctxStack(level)
      (c eq this)
    def detached2 =
      ctxStack(level) eq this
