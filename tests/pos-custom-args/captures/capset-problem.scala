import language.experimental.captureChecking
import caps.use

// Some capabilities that should be used locally
trait Async

case class Box[+T](get: T)

def useBoxedAsync[C^](x: Box[Async^{C}]): Unit = ???
def foo[C](x: Box[C]): Unit = ???

def test(): Unit =

  val t3 = useBoxedAsync(_)
  val _: Box[Async^{}] -> Unit = t3
  val t4 = foo(_)
  val _: Box[Any] -> Unit = t4