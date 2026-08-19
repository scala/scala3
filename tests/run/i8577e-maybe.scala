//> using options -Yexplicit-nulls
import language.experimental.magic
object Macro:
  opaque type StrCtx = StringContext
  def apply(ctx: StringContext): StrCtx = ctx
  def unapply(ctx: StrCtx): StringContext? = ctx

extension (ctx: StringContext) def mac: Macro.StrCtx = Macro(ctx)
extension [T] (inline ctx: Macro.StrCtx) inline def unapplySeq[U](inline input: (T, U)): Seq[(T, U)]? =
  Seq(input)

@main def Test: Unit =
  val mac"$x" = (1, 2)
  val x2: (Int, Int) = x
  assert(x == (1, 2))

  val mac"$y" = (1, "a")
  val y2: (Int, String) = y
  assert(y == (1, "a"))
