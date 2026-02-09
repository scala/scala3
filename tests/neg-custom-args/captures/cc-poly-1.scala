import language.experimental.captureChecking
import caps.{CapSet, SharedCapability}

object Test:

  class C extends SharedCapability
  class D

  def f[X^](x: D^{X}): D^{X} = x

  def test(c1: C, c2: C) =
    f[Any](D()) // error
    f[String](D()) // error
