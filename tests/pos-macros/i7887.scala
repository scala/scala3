def typed[A](using s: quoted.Scope)(using t: s.Type[A]): Unit = {
  '{
    type T = $t
    ${'{???}.cast[T]}
  }
}
