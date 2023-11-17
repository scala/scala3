trait Cap { def use(): Int }

def test1(io: Cap^): Unit = {
  type Id[X] = [T] -> (op: X ->{io} T) -> T

  val x: Id[Cap]^{io} = ???
  x(cap => cap.use())  // ok
}

def test2(io: Cap^): Unit = {
  type Id[X] = [T] -> (op: (x: X) ->{io} T) -> T

  val x: Id[Cap^{io}] = ???
  x(cap => cap.use())
  // should work when the expected type is a dependent function
}

def test3(io: Cap^): Unit = {
  type Id[X] = [T] -> (op: (x: X) ->{} T) -> T

  val x: Id[Cap^{io}] = ???
  x(cap => cap.use()) // error
}
