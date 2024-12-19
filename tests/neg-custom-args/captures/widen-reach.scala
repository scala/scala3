import language.experimental.captureChecking

trait IO

trait Foo[+T]:
  val foo: IO^ -> T

trait Bar extends Foo[IO^]: // error
  val foo: IO^ -> IO^ = x => x

def test(x: Foo[IO^]): Unit =
  val y1: Foo[IO^{x*}] = x
  val y2: IO^ -> IO^ = y1.foo      // error
  val y3: IO^ -> IO^{x*} = y1.foo  // error