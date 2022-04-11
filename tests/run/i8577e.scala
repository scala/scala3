import scala.quoted._

object Macro:
  opaque type StrCtx = StringContext
  def apply(ctx: StringContext): StrCtx = ctx
  def unapply(ctx: StrCtx): Option[StringContext] = Some(ctx)

extension (ctx: StringContext) def mac: Macro.StrCtx = Macro(ctx)
extension [T] (inline ctx: Macro.StrCtx) inline def unapplySeq[U](inline input: (T, U)): Option[Seq[(T, U)]] =
  Some(Seq(input))

@main def Test: Unit =
  val mac"$x" = (1, 2)
  val x2: (Int, Int) = x
  assert(x == (1, 2))

  val mac"$y" = (1, "a")
  val y2: (Int, String) = y
  assert(y == (1, "a"))
