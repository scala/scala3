def typed[A](using t: quoted.Staged[A], qctx: quoted.QuoteContext): Unit = {
  import qctx.tasty._
  '{
    type T = $t
    ${'{???}.cast[T]}
  }
}
