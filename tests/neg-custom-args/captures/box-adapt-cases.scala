trait Cap { def use(): Int }

def test1(): Unit = {
  type Id[X] = [T] -> (op: X => T) -> T

  val x: Id[{*} Cap] = ???
  x(cap => cap.use())  // error
}

def test2(io: {*} Cap): Unit = {
  type Id[X] = [T] -> (op: X -> T) -> T

  val x: Id[{io} Cap] = ???
  x(cap => cap.use())  // error
}

def test3(io: {*} Cap): Unit = {
  type Id[X] = [T] -> (op: {io} X -> T) -> T

  val x: Id[{io} Cap] = ???
  x(cap => cap.use())  // ok
}

def test4(io: {*} Cap, fs: {*} Cap): Unit = {
  type Id[X] = [T] -> (op: {io} X -> T) -> T

  val x: Id[{io, fs} Cap] = ???
  x(cap => cap.use())  // error
}
