trait Cap { def use(): Int }

def test1(io: Cap^): Unit = {
  type Id[X] = [T] -> (op: X ->{io} T) -> T

  val x: Id[Cap^{io}] = ???
  val f: (Cap^) -> Unit = ???
  x(f)  // error

  val g: (Cap^{io}) -> Unit = ???
  x(g)  // ok
}

def test2(io: Cap^): Unit = {
  type Id[X] = [T] -> (op: X => T) -> T

  val x: Id[Cap^] = ???
  val f: Cap^{io} -> Unit = ???
  x(f)  // error
}
