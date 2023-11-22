trait Cap

def test1(io: Cap^) = {
  type Op[X] = [T] -> Unit -> X
  val f: Op[Cap^{io}] = ???
  val x: [T] -> Unit -> Cap^{io} = f  // error
}

def test2(io: Cap^) = {
  type Op[X] = [T] -> Unit -> X^{io}
  val f: Op[Cap^{io}] = ???
  val x: Unit -> Cap^{io} = f[Unit]  // error
  val x1: Unit ->{io} Cap^{io} = f[Unit]  // ok
}
