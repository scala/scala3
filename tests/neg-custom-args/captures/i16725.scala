import language.experimental.captureChecking
class IO extends caps.Capability:
  def brewCoffee(): Unit = ???
def usingIO[T](op: IO => T): T = ???

type Wrapper[T] = [R] -> (f: T => R) -> R
def mk[T](x: T): Wrapper[T] = [R] => f => f(x)
def useWrappedIO(wrapper: Wrapper[IO]): () -> Unit =
  () =>
    wrapper: io =>  // error
      io.brewCoffee()
def main(): Unit =
  val escaped = usingIO(io => useWrappedIO(mk(io)))
  escaped()  // boom
