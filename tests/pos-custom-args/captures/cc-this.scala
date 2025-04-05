
import caps.consume
import caps.unsafe.unsafeAssumeSeparate

class Cap extends caps.Capability

def eff(using Cap): Unit = ()

def test(using @consume cc: Cap) =

  class C(val x: () => Int):
    val y: C^ = this

  def f = () =>
    eff
    1

  def c1 = new C(f)
  def c2 = c1
  def c3 = unsafeAssumeSeparate:
    c2.y // unsafe since c3's inferred type is
         // C{val x: () ->{<fresh hiding {cc}>} Int}^{cc}
         // and that type hides non-local cc.
         // c.f. test2 in neg test cc-this.scala
  val c4: C^ = c3
  val _ = c3: C^
