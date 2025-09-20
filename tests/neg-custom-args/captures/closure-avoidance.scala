trait A
trait B

def test =
  val f = (a: A^) =>
    val b: B^ = ???
    b

  val f1: A^ => B^ = (a: A^) => // error -- can we make this work as for f2?
    val b: B^ = ???
    b

  val f2: (x: A^) => B^ = (a: A^) =>
    val b: B^ = ???
    b   // ok

  val g = (a: A^) =>
    println("")
    (??? : B^)

  val test = g

  val g1: A^ => B^ = (a: A^) => // error -- can we make this work as for g2?
    println("")
    (??? : B^)

  val g2: (x: A^) => B^ = (a: A^) =>
    println("")
    (??? : B^) // ok

