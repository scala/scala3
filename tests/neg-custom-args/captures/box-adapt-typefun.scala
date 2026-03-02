trait Cap { def use(): Int }

def test1(io: Cap^): Unit = {
  class Op[X](val value: [T] -> X -> Unit)
  val f: [T] -> (Cap^{io}) -> Unit = ???
  val op: Op[Cap^{io}] = Op(f) // was error, now ok
}

def test2(io: Cap^): Unit = {
  class Lazy[X](val value: [T] -> Unit -> X)
  val f: Lazy[Cap^{io}] = ???
  val test: [T] -> Unit -> (Cap^{io}) = f.value // error
}
