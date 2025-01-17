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

  val g = () => println(c.procs.head)
  val _: () ->{c.procs*} Unit = g
