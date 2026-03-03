import caps.fresh
trait A
trait B

def test =
  val f = (a: A^) =>
    val b: B^ = ???
    b

  val f1: A^ => B^{fresh} = (a: A^) => // OK
    val b: B^ = ???
    b

  val f2: (x: A^) => B^{fresh} = (a: A^) =>
    val b: B^ = ???
    b   // ok

  val g = (a: A^) =>
    println("")
    (??? : B^)

  val test = g

  val g1: A^ => B^{fresh} = (a: A^) => // OK
    println("")
    (??? : B^)

  val g2: (x: A^) => B^{fresh} = (a: A^) =>
    println("")
    (??? : B^) // ok
