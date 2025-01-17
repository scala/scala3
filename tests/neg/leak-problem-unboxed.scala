import language.experimental.captureChecking
import caps.unbox

// Some capabilities that should be used locally
trait Async:
  //  some method
  def read(): Unit
def usingAsync[X](op: Async^ => X): X = ???

case class Box[+T](get: T)

def useBoxedAsync(@unbox x: Box[Async^]): Unit =
  val t0 = x
  val t1 = t0.get // ok
  t1.read()

def useBoxedAsync1(@unbox x: Box[Async^]): Unit = x.get.read() // ok

def test(): Unit =

  val f: Box[Async^] => Unit = (x:  Box[Async^]) => useBoxedAsync(x) // error
  val _: Box[Async^] => Unit = useBoxedAsync(_) // error
  val _: Box[Async^] => Unit = useBoxedAsync // error
  val _ = useBoxedAsync(_) // error
  val _ = useBoxedAsync // error

  def boom(x: Async^): () ->{f} Unit =
    () => f(Box(x))

  val leaked = usingAsync[() ->{f} Unit](boom)

  leaked()  // scope violation