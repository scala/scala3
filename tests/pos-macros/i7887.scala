def typed[A](using t: quoted.Type[A], q: quoted.Quotes): Unit = {
  import q.reflect._
  '{
    type T = A
    ${'{???}.asExprOf[T]}
  }
}
