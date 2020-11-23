def typed[A](using t: quoted.Type[A], qctx: quoted.Quotes): Unit = {
  import qctx.reflect._
  '{
    type T = A
    ${'{???}.asExprOf[T]}
  }
}
