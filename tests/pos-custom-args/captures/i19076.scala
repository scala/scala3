import language.experimental.captureChecking
trait IO
def main(a: IO^): Unit =
  def foo[X <: IO^{a}](x: X): IO^{a} = x  // now ok
