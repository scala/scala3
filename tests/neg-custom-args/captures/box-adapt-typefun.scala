trait Cap { def use(): Int }

def test1(io: Cap^): Unit = {
  type Op[X] = [T] -> X -> Unit
  val f: [T] -> (Cap^{io}) -> Unit = ???
  val op: Op[Cap^{io}] = f // error
}

def test2(io: Cap^): Unit = {
  type Lazy[X] = [T] -> Unit -> X
  val f: Lazy[Cap^{io}] = ???
  val test: [T] -> Unit -> (Cap^{io}) = f // error
}
