// a.scala
import language.experimental.captureChecking

trait A:
  def f[cap C](x: AnyRef^{C}): Unit
