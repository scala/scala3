import language.experimental.captureChecking
import caps.use

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
  val t0: Box[Async^] => Unit = x => useBoxedAsync(x) // TODO hould be error!

  val t1: Box[Async^] => Unit = useBoxedAsync(_) // TODO should be error!
  val t2: Box[Async^] => Unit = useBoxedAsync // TODO should be error!
  val t3 = useBoxedAsync(_) // was error, now ok
  val t4 = useBoxedAsync // was error, now ok

  def boom(x: Async^): () ->{f} Unit =
    () => f(Box(x))

  val leaked = usingAsync[() ->{f} Unit](boom)

  leaked()  // scope violation
