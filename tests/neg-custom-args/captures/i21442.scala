import language.experimental.captureChecking
trait IO:
  def use(): Unit
case class Boxed[+T](unbox: T)

// `foo` is a function that unboxes its parameter
// and uses the capability boxed inside the parameter.
def foo(x: Boxed[IO^]): Unit =
  val io = x.unbox // error: local reach capability {x*} leaks
  io.use()

// `bar` is a function that does the same thing in a
// slightly different way.
// But, no type error reported.
def bar(x: Boxed[IO^]): Unit =
  val x1: Boxed[IO^] = x
  val io = x1.unbox // error
  io.use()
