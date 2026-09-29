import language.experimental.captureChecking

// Some capabilities that should be used locally
trait Async:
  //  some method
  def read(): Unit
def usingAsync[X](op: Async^ => X): X = ???

case class Box[+T](get: T)

def test(): Unit =
  def useBoxedAsync[C^](x: Box[Async^{C}]): Unit =
    val t0 = x
    val t1 = t0.get
    t1.read()

  def useBoxedAsync1[C^](x: Box[Async^{C}]): Unit = x.get.read()

  val xs: Box[Async^] = ???
  val xsLambda = () => useBoxedAsync(xs) // was error now ok
  val _: () ->{xs*} Unit = xsLambda
  val _: () -> Unit = xsLambda // error
