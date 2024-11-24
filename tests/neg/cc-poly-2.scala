import language.experimental.captureChecking
import caps.{CapSet, Capability}

object Test:

  class C extends Capability
  class D

  def f[X^](x: D^{X^}): D^{X^} = x

  def test(c1: C, c2: C) =
    val d: D^ = D()
    // f[Nothing](d) // already rule out at typer
    f[CapSet^{c1}](d) // error
    val x = f(d)
    val _: D^{c1} = x // error
