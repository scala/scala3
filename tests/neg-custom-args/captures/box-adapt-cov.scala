trait Cap

def test1(io: Cap^) = {
  class Op[+X](val value: [T] -> Unit -> X)
  val f: Op[Cap^{io}] = ???
  val x: [T] -> Unit -> Cap^{io} = f.value  // error
}

def test2(io: Cap^) = {
  class Op[+X](val value: [T] -> Unit -> X^{io})
  val f: Op[Cap^{io}] = ???
  val x: Unit -> Cap^{io} = f.value[Unit]  // error
  val x1: Unit ->{io} Cap^{io} = f.value[Unit]  // ok
}
