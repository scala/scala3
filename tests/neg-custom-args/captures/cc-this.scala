

class Cap extends caps.ExclusiveCapability

def eff(using Cap): Unit = ()

def test(using Cap) =

  class C(val x: () => Int):
    val y: C = this // error

  class C2(val x: () => Int): // error
    this: C2 =>

  class C3:
    this: C3 =>
    val x: Object = this

  class C4(val f: () => Int) extends C3 // error

// The following is a variation of pos/cc-this.scala
def test2(using consume cc: Cap) =

  class C(val x: () => Int):
    val y: C^ = this

  def f = () =>
    eff(using cc)
    1

  def c1 = new C(f)
  def c2 = c1
  def c3 = c2.y // was error, now OK
  val c4: C^ = c3
  val _ = c3: C^
