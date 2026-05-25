//> using options -language:experimental.captureChecking

class IO extends caps.SharedCapability:
  def println(msg: String): Unit = ()

def test(io: IO): Unit =
  class FunSuite1 extends FunSuite:
    self: FunSuite1^ =>
    def run(): Unit = io.println("Hello")
