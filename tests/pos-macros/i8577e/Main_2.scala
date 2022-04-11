package i8577

def main: Unit =
  extension (ctx: StringContext) def mac: Macro.StrCtx = Macro(ctx)
  extension [T] (inline ctx: Macro.StrCtx) inline def unapplySeq[U](inline input: U): Option[Seq[U]] =
    ${ implUnapply('ctx, 'input) }

  val mac"$x" = 1
  assert(x == 1)
