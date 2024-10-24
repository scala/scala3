
def test() =
  val f: () => () => Unit = ???
  val x = () =>
    val z1: () => Unit = f()
    val z2 = z1()
    z2
  val _: () ->{f} Unit = x
  ()
