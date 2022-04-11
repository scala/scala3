import scala.quoted._

object Macro:
  opaque type StrCtx = StringContext
  def apply(ctx: StringContext): StrCtx = ctx
  def unapply(ctx: StrCtx): Option[StringContext] = Some(ctx)

extension (ctx: StringContext) def mac: Macro.StrCtx = Macro(ctx)
extension (inline ctx: Macro.StrCtx) transparent inline def unapplySeq(inline input: String): Option[Seq[Any]] =
  Some(Seq(123))

@main def Test: Unit =
  "abc" match
    case mac"$x" =>
      val y: Int = x
      assert(x == 123)
