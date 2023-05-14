package i8577

def main: Unit =
  extension (ctx: StringContext) def mac: Macro.StrCtx = Macro(ctx)
  extension (inline ctx: Macro.StrCtx) inline def unapplySeq(inline input: Int): Option[Seq[Int]] =
    ${ implUnapply('ctx, 'input) }

  val mac"$x" = 1
  assert(x == 1)
