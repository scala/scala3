import language.experimental.captureChecking
import annotation.retains
import annotation.retainsCap

object Test:

  def f[A <: Matchable @retains[caps.cap.type]](x: A): Matchable = x // error

  def g[A <: Matchable @retainsCap](x: A): Matchable = x // error

