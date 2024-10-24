import language.experimental.captureChecking
import caps.use

// Some capabilities that should be used locally
trait Async:
  //  some method
  def read(): Unit
def usingAsync[X](op: Async^ => X): X = ???

case class Box[+T](get: T)

def useBoxedAsync(x: Box[Async^ @use]): Unit =
  val t0 = x
  val t1 = t0.get // ok
  t1.read()

def useBoxedAsync1(x: Box[Async^ @use]): Unit = x.get.read() // ok

def test(): Unit =

  val f: Box[Async^] => Unit = (x:  Box[Async^]) => useBoxedAsync(x) // error
  val _: Box[Async^] => Unit = useBoxedAsync(_) // error
  val _: Box[Async^] => Unit = useBoxedAsync // error
  val _ = (x:  Box[Async^]) => useBoxedAsync(x) // error
  val _ = useBoxedAsync(_) // was error, now OK since @use is inferred
  val _ = useBoxedAsync // was error, now OK since @use is inferred

  def boom(x: Async^): () ->{f} Unit =
    () => f(Box(x))

  val leaked = usingAsync[() ->{f} Unit](boom)

  leaked()  // scope violation