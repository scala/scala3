def typed[A](using t: quoted.Type[A], qctx: quoted.QuoteContext): Unit = {
  import qctx.tasty._
  '{
    type T = t.T
    ${'{???}.cast[T]}
  }
}
