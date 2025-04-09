import language.experimental.captureChecking
import caps.{CapSet, Capability}

object Test:

  class C extends Capability
  class D

  def f[cap X](x: D^{X}): D^{X} = x

  def test(c1: C, c2: C) =
    f[Any](D()) // error
    f[String](D()) // error
