import language.experimental.captureChecking
import caps.use

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
  def useBoxedAsync(@use x: Box[Async^]): Unit =
    val t0 = x
    val t1 = t0.get
    t1.read()

  def useBoxedAsync1(@use x: Box[Async^]): Unit = x.get.read()

  val xs: Box[Async^] = ???
  val xsLambda = () => useBoxedAsync(xs) // error
  val _: () ->{xs*} Unit = xsLambda
  val _: () -> Unit = xsLambda // error

  val useBoxedAsync2 = (x: Box[Async^]) =>
    val t0 = x
    val t1 = x.get // error
    t1.read()

  val xsLambda2 = () => useBoxedAsync2(xs)
  val _: () ->{xs*} Unit = xsLambda2
  val _: () -> Unit = xsLambda2

  val f: Box[Async^] => Unit = (x:  Box[Async^]) => useBoxedAsync(x) // error

  def boom(x: Async^): () ->{f} Unit =
    () => f(Box(x))

  val leaked = usingAsync[() ->{f} Unit](boom)

  leaked()  // scope violation