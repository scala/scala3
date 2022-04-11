package i8577

def main: Unit =
  extension (ctx: StringContext) def mac: Macro.StrCtx = Macro(ctx)
  extension [T] (inline ctx: Macro.StrCtx) inline def unapplySeq[U](inline input: (T, U)): Option[Seq[(T, U)]] =
    ${ implUnapply('ctx, 'input) }

  val mac"$x" = (1, 2)
  assert(x == (1, 2))

  val mac"$y" = (1, "a")
  assert(y == (1, "a"))
