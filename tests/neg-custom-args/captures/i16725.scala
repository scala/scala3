import language.experimental.captureChecking
class IO extends caps.SharedCapability:
  def brewCoffee(): Unit = ???
def usingIO[T](op: IO => T): T = ???

class Wrapper[T](val value: [R] -> (f: T => R) -> R)
def mk[T](x: T): Wrapper[T] = Wrapper([R] => f => f(x))
def useWrappedIO(wrapper: Wrapper[IO]): () -> Unit =
  () =>
    wrapper.value: io => // error
      io.brewCoffee()
def main(): Unit =
  val escaped = usingIO(io => useWrappedIO(mk(io)))
  escaped()  // boom
