import language.experimental.captureChecking

// Some capabilities that should be used locally
trait Async:
  //  some method
  def read(): Unit
def usingAsync[X](op: Async^ => X): X = ???

case class Box[+T](get: T)

def useBoxedAsync[C^](x: Box[Async^{C}]): Unit =
  val t0 = x
  val t1 = t0.get // ok
  t1.read()

def useBoxedAsync1[C^](x: Box[Async^{C}]): Unit = x.get.read() // ok

def test(): Unit =

  val f: Box[Async^] => Unit = (x: Box[Async^]) => useBoxedAsync(x) // error
  val f0: Box[Async^] => Unit = x => useBoxedAsync(x) // // error

  val f1: Box[Async^] => Unit = useBoxedAsync(_) // error
  val f2: Box[Async^] => Unit = useBoxedAsync // error
  val f3 = useBoxedAsync(_) // was error, now ok, but bang below fails
  val f4 = useBoxedAsync // was error, now ok, but bang2 below fails

  def boom(x: Async^): () ->{f} Unit =
    () => f(Box(x))

  val leaked = usingAsync[() ->{f} Unit](boom)

  leaked()  // was scope violation

  def bang(x: Async^) =
    () => f3(Box(x)) // error

  def bang2(x: Async^) =
    () => f3(Box(x)) // error

