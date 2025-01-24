import language.experimental.captureChecking
import scala.caps.CapSet

class B extends A:
  def f[C^](x: AnyRef^{C^}): Unit = ???
