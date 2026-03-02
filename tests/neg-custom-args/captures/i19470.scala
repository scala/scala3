import language.experimental.captureChecking
trait IO
case class Inv[X](x: X)

def foo(x: IO^): Inv[IO^{x}] = Inv(x)

def main(io: IO^): Unit =
  def test(f: () => IO^) =
    List(foo(f())) // error
