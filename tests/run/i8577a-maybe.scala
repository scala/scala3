//> using options -Yexplicit-nulls
import language.experimental.errorHandling
object Macro:
  opaque type StrCtx = StringContext
  def apply(ctx: StringContext): StrCtx = ctx
  def unapply(ctx: StrCtx): StringContext? = ctx

extension (ctx: StringContext) def mac: Macro.StrCtx = Macro(ctx)
extension (inline ctx: Macro.StrCtx) inline def unapplySeq(inline input: Int): Seq[Int]? =
  Seq(input)

@main def Test: Unit =
  val mac"$x" = 1.runtimeChecked
  val y: Int = x
  assert(x == 1)
