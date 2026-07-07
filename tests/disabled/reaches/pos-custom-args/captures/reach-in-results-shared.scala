import language.experimental.captureChecking
import caps.{any, Shared, SharedCapability}

class IO extends SharedCapability
class C

def test(io: IO^): Unit =
  val f = (x: () ->{any.only[Shared]} IO^) => x()
  val g = (x: () ->{any.only[Shared]} IO^{any.only[Shared]}) => x()
  val h = (x: () ->{any.only[Shared]} C^{any.only[Shared]}) => x()
/*
  val _: (x: () ->{any.only[Shared]} IO^) => IO^{x} = (x: () ->{any.only[Shared]} IO^) => f(x)
  val _: (x: () ->{any.only[Shared]} IO^) => IO^{x} = f//(x: () => IO^) => f(x)
  def g(x: IO^ => IO^) = x(io)
  def h(x: (y: IO^) => IO^) = x(io)

  val a: () -> IO^ = ???
  val _: () -> IO^{a*} = a
  val b: (x: IO^) -> IO^ = ???
  val _: (x: IO^) -> IO^ = b
  val c: IO^ -> IO^ = ???
  val cc: IO^ -> IO^{c*} = c

def testByName(io: IO^): Unit =
  def f(x: => () => IO^) = x()
  def g(x: => IO^ => IO^) = x(io)
  def h(x: => (y: IO^) => IO^) = x(io)
*/