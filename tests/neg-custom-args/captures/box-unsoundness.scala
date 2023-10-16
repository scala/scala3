//@annotation.capability
class CanIO { def use(): Unit = () }
def use[X](x: X): (op: X -> Unit) -> Unit = op => op(x)
def test(io: CanIO^): Unit =
  val f: (CanIO^ => Unit) -> Unit = use[CanIO^](io)  //
  val _: (CanIO^ => Unit) -> Unit = f  // error

  val g1 = () => f(x => x.use())  // error

  val a1 = f(x => x.use())
  val a2 = () => f(x => x.use())
  val g2: () -> Unit = a2        // error
  // was UNSOUND: g uses the capability io but has an empty capture set