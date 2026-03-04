
trait Cap { def use(): Int }

def test1(): Unit = {
  class Id[X](val value: [T] -> (op: X => T) -> T)

  val x: Id[Cap^] = ???
  x.value(cap => cap.use()) // error
}

def test2(io: Cap^): Unit = {
  class Id[X](val value: [T] -> (op: X -> T) -> T)

  val x: Id[Cap^{io}] = ???
  x.value(cap => cap.use())  // error
}

def test3(io: Cap^): Unit = {
  class Id[X](val value: [T] -> (op: X ->{io} T) -> T)

  val x: Id[Cap^{io}] = ???
  x.value(cap => cap.use())  // ok
}

def test4(io: Cap^, fs: Cap^): Unit = {
  class Id[X](val value: [T] -> (op: X ->{io} T) -> T)

  val x: Id[Cap^{io, fs}] = ???
  x.value(cap => cap.use())  // error
}
