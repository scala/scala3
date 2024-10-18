import caps.use

type Proc = () => Unit

def f(xs: List[Proc] @use) = ???
def g(xs: List[Proc]) = ???

def Test(ioProcs: List[Proc @use]) =
  val ff = () => println(f(ioProcs))  // ok
  ()

def Test2(ioProcs: List[Proc]) =
  val _ = () => println(f(ioProcs))  // error
  val gg = () => println(g(ioProcs))  // ok
  ()

