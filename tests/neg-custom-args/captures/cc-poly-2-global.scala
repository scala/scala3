import language.experimental.captureChecking
import caps.{CapSet, SharedCapability}

object Test:

  class C extends SharedCapability
  class D

  def f[X^](x: D^{X}): D^{X} = x

  val c1: C = C()
  val c2: C = C()

  def test() =
    val d: D^ = D()
    // f[Nothing](d) // already ruled out at typer
    f[CapSet^{c1}](d) // error
    val x = f(d)
    val _: D^{c1} = x // error
