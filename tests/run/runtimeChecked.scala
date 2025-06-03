//> using options -Werror -source:future -experimental

val xs: List[Any] = List(1: Any)

@main
def Test: Unit =
  val head = xs.runtimeChecked match {
    // tests/neg/runtimeChecked.scala asserts that @unchecked is
    // still needed for unsound type tests.
    case is: ::[Int @unchecked] => is.head
  }
  assert(head == 1)
