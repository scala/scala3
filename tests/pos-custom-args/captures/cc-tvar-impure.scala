import language.experimental.captureChecking
trait IO
def main(io: IO^): Unit =
  def f[X <: IO^{io}](x: X): IO^{io} = x
