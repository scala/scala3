//> using options -Werror -source:future -experimental

val xs: List[Any] = List(1: Any)

@main
def Test: Unit =
  val head :: _ = xs.runtimeChecked
  assert(head == 1)
