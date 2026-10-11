import language.experimental.captureChecking

trait IO

trait Foo[+T]:
  val foo: IO^ -> T

trait Bar extends Foo[IO^]: // error
  val foo: IO^ -> IO^ = x => x // error

def test[c^](x: Foo[IO^{c}]): Unit =
  val y1: Foo[IO^{c}] = x
  val y2: IO^ -> IO^ = y1.foo      // error
  val y3: IO^ -> IO^{c} = y1.foo  // error