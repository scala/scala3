import language.experimental.captureChecking

// Some capabilities that should be used locally
trait Async:
  //  some method
  def read(): Unit
def usingAsync[X](op: Async^ => X): X = ???

case class Box[+T](get: T)

def useBoxedAsync(x: Box[Async^]): Unit =
  val t0 = x
  val t1 = t0.get // error
  t1.read()

def useBoxedAsync1(x: Box[Async^]): Unit = x.get.read() // error

def test(): Unit =
  val useBoxedAsync2 = (x: Box[Async^]) =>
    val t0 = x
    val t1 = x.get // error
    t1.read()

  val f: Box[Async^] => Unit = (x:  Box[Async^]) => useBoxedAsync(x)

  def boom(x: Async^): () ->{f} Unit =
    () => f(Box(x))

  val leaked = usingAsync[() ->{f} Unit](boom)

  leaked()  // scope violation