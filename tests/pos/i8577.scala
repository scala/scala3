package i8577

type A; given A: A = ???;
type B; given B: B = ???;
type C; given C: C = ???;
type D; given D: D = ???;
type E; given E: E = ???;
type F; given F: F = ???;


object Macro:
  opaque type StrCtx = StringContext
  def apply(ctx: StringContext): StrCtx = ctx
  def unapply(ctx: StrCtx): Option[StringContext] = Some(ctx)

def main: Unit =
  extension (ctx: StringContext) def mac: Macro.StrCtx = Macro(ctx)
  extension [T] (using A)(inline ctx: Macro.StrCtx)(using B) inline def unapplySeq[U](using C)(inline input: T)(using D)(using F): Option[Seq[T]] = ???

  (??? : Int) match
    case mac"${x}" => 1
