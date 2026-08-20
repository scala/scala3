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

  val f3 = useBoxedAsync(_) // was error, now ok, but bang below fails

  def bang(x: Async^) =
    () => f3(Box(x)) // error

  def bang2(x: Async^) =
    () => f3(Box(x)) // error

