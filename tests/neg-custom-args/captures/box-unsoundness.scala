@annotation.capability class CanIO { def use(): Unit = () }
def use[X](x: X): (op: X -> Unit) -> Unit = op => op(x)
def test(io: CanIO): Unit =
  val f = use[CanIO](io)
  val g: () -> Unit = () => f(x => x.use())  // error
  // was UNSOUND: g uses the capability io but has an empty capture set