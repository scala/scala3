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

def test3(io: Cap^, fs: Cap^) = {
  class Op[+X](val value: [T] -> Unit -> X^{io})
  def make[X](x: X): Op[X] = new Op[X]([T] => (_: Unit) => x)
  val f: Op[Cap^{fs}] = make[Cap^{fs}](fs)
  // f.value[Unit] behaves like (_: Unit) => fs, so the returned function
  // must capture fs to return it when called.
  val x: Unit ->{io} Cap^{io, fs} = f.value[Unit]  // error
  val x1: Unit ->{fs} Cap^{io, fs} = f.value[Unit]
}
