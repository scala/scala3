trait Cap

def test1(io: {*} Cap) = {
  type Op[X] = [T] -> Unit -> X
  val f: Op[{io} Cap] = ???
  val x: [T] -> Unit -> ({io} Cap) = f  // error
}

def test2(io: {*} Cap) = {
  type Op[X] = [T] -> Unit -> {io} X
  val f: Op[{io} Cap] = ???
  val x: Unit -> ({io} Cap) = f[Unit]  // error
  val x1: {io} Unit -> ({io} Cap) = f[Unit]  // ok
}
