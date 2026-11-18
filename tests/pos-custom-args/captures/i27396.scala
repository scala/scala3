import language.experimental.captureChecking
class IO extends caps.SharedCapability:
  def use(): Unit = ()
def test(x: IO, y: IO) =
  val a: IO^{y, x} = x
  val b: (IO^{y})^{x} = x
  type F = () ->{y} Unit
  val g: F^{x} = () => x.use()
