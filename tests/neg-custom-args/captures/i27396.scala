import language.experimental.captureChecking
class IO extends caps.SharedCapability:
  def use(): Unit = ()
def test(x: IO, y: IO, z: IO) =
  val b: (IO^{y})^{x} = z                   // error
  type F = () ->{y} Unit
  val g: F^{x} = () => z.use()              // error
