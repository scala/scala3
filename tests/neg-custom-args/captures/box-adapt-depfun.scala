trait Cap { def use(): Int }

def test1(io: Cap^): Unit = {
  class Id[X](val value: [T] -> (op: X ->{io} T) -> T)

  val x: Id[Cap]^{io} = ???
  x.value(cap => cap.use())  // ok
}

def test2(io: Cap^): Unit = {
  class Id[X](val value: [T] -> (op: (x: X) ->{io} T) -> T)

  val x: Id[Cap^{io}] = ???
  x.value(cap => cap.use())
  // should work when the expected type is a dependent function
}

def test3(io: Cap^): Unit = {
  class Id[X](val value: [T] -> (op: (x: X) ->{} T) -> T)

  val x: Id[Cap^{io}] = ???
  x.value(cap => cap.use()) // error
}
