package i8577

def main: Unit =
  extension (ctx: StringContext) def mac: Macro.StrCtx = Macro(ctx)
  extension [T] (inline ctx: Macro.StrCtx) inline def unapplySeq(inline input: T): Option[Seq[T]] =
    ${ implUnapply('ctx, 'input) }

  val mac"$x" = 1
  assert(x == 1)
