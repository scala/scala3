trait Cap { def use(): Int }

def test1(io: {*} Cap): Unit = {
  type Op[X] = [T] -> X -> Unit
  val f: [T] -> ({io} Cap) -> Unit = ???
  val op: Op[{io} Cap] = f // error
}

def test2(io: {*} Cap): Unit = {
  type Lazy[X] = [T] -> Unit -> X
  val f: Lazy[{io} Cap] = ???
  val test: [T] -> Unit -> ({io} Cap) = f // error
}
