import language.experimental.namedTuples

class IO

class C(val f: IO^):
  val procs: List[Proc] = ???

type Proc = () => Unit

def test(io: IO^) =
  val c = C(io)
  val f = () => println(c.f)
  val _: () ->{c.f} Unit = f

  val x = c.procs
  val _: List[() ->{c.procs*} Unit] = x

  val g = () => println(c.procs.head) // error, local reach capability c.procs* leaks
  val _: () ->{c.procs*} Unit = g

  val cc: C { val f: IO^{io}; val procs: List[() ->{io} Unit] }^{io} =
    ???

  val gg = () => println(cc.procs.head) // OK, since cc.procs* has {io} as underlying capture set
  val _: () ->{io} Unit = gg
