def typed[A](using t: quoted.Type[A], q: quoted.Quotes): Unit = {
  import q.reflect.*
  '{
    type T = A
    ${'{???}.asExprOf[T]}
  }
}
