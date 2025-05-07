// a.scala
import language.experimental.captureChecking
import scala.caps.CapSet

trait A:
  def f[C^](x: AnyRef^{C^}): Unit
