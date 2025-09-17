import language.experimental.captureChecking
import caps.{cap, Shared}

class IO

def test(io: IO^): Unit =
  val f = (x: () ->{cap.only[Shared]} IO^{cap.only[Shared]}) => x()

  val _: (x: () ->{cap.only[Shared]} IO^{cap.only[Shared]}) => IO^{x} = (x: () ->{cap.only[Shared]} IO^{cap.only[Shared]}) => f(x)
  val _: (x: () ->{cap.only[Shared]} IO^{cap.only[Shared]}) => IO^{x} = f//(x: () => IO^) => f(x)
/*
  val f = (x: () => IO^) => x()
  val _: (x: () => IO^) => IO^{x} = (x: () => IO^) => f(x)
  val _: (x: () => IO^) => IO^{x} = f//(x: () => IO^) => f(x)
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