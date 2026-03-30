class CanIO { def use(): Unit = () }
def use[X](x: X): (op: X -> Unit) -> Unit = op => op(x)
val io: CanIO^ = CanIO()
def test: Unit =
  val f: (CanIO^ => Unit) -> Unit = use[CanIO^](io)  // error
  val _: (CanIO^ => Unit) -> Unit = f

  val g1 = () => f(x => x.use())

  val a1 = f(x => x.use())
  val a2 = () => f(x => x.use())
  val g2: () -> Unit = a2
  // was UNSOUND: g uses the capability io but has an empty capture set
