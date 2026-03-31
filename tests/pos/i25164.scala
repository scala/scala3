import scala.language.experimental.modularity

trait Ctx {
  type State
}

sealed trait Base(tracked val ctx: Ctx)

object Base {
  abstract class Sub(tracked override val ctx: Ctx) extends Base(ctx) {
    // Crash: function returning applied constructor type Base(ctx)
    def f: Int => Base(ctx)
  }
}
