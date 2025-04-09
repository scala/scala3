import language.experimental.captureChecking
import annotation.experimental
import caps.{CapSet, Capability}

@experimental object Test:

  class C extends Capability
  class D

  def f[cap X](x: D^{X}): D^{X} = x
  def g[cap X](x: D^{X}, y: D^{X}): D^{X} = x
  def h[cap X](): D^{X} = ???

  def test(c1: C, c2: C) =
    val d: D^{c1, c2} = D()
    val x = f[{c1, c2}](d)
    val _: D^{c1, c2} = x
    val d1: D^{c1} = D()
    val d2: D^{c2} = D()
    val y = g(d1, d2)
    val _: D^{d1, d2} = y
    val _: D^{c1, c2} = y
    val z = h()



