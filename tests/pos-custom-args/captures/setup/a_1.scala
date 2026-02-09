// a.scala
import language.experimental.captureChecking

trait A:
  def f[C^](x: AnyRef^{C}): Unit
