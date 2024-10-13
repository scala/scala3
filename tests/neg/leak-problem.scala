import language.experimental.captureChecking

// Some capabilities that should be used locally
trait Async:
  //  some method
  def read(): Unit
def usingAsync[X](op: Async^ => X): X = ???

case class Box[+T](get: T)

def useBoxedAsync(x: Box[Async^]): Unit =
  val t0 = x
  val t1 = t0.get // now ok
  t1.read()

def useBoxedAsync1(x: Box[Async^]): Unit = x.get.read() // now ok

def test(): Unit =
  val xs: Box[Async^] = ???
  val xsLambda = () => useBoxedAsync(xs)
  val _: () ->{xs*} Unit = xsLambda
  val _: () -> Unit = xsLambda // error

  val useBoxedAsync2 = (x: Box[Async^]) =>
    val t0 = x
    val t1 = x.get
    t1.read()

  val xsLambda2 = () => useBoxedAsync2(xs)
  val _: () ->{xs*} Unit = xsLambda
  val _: () -> Unit = xsLambda // error

  val f: Box[Async^] => Unit = (x:  Box[Async^]) => useBoxedAsync(x)

  def boom(x: Async^): () ->{f} Unit =
    val ff = () => f(Box(x))  // error
    val _: () ->{f, x} Unit = ff
    ff

  val leaked = usingAsync[() ->{f} Unit](boom)

  leaked()  // scope violation