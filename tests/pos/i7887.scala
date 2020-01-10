def typed[A](given t: quoted.Type[A], qctx: quoted.QuoteContext): Unit = {
  import qctx.tasty.{given, _}
  '{
    type T = $t
    ${'{???}.cast[T]}
  }
}
