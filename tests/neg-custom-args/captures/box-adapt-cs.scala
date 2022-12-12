trait Cap { def use(): Int }

def test1(io: {*} Cap): Unit = {
  type Id[X] = [T] -> (op: {io} X -> T) -> T

  val x: Id[{io} Cap] = ???
  val f: ({*} Cap) -> Unit = ???
  x(f)  // ok
  // actual: {*} Cap -> Unit
  // expected: {io} box {io} Cap -> Unit
}

def test2(io: {*} Cap): Unit = {
  type Id[X] = [T] -> (op: {*} X -> T) -> T

  val x: Id[{*} Cap] = ???
  val f: ({io} Cap) -> Unit = ???
  x(f)  // error
}
