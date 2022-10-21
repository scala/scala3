@annotation.capability class Cap

def eff(using Cap): Unit = ()

def test(using Cap) =

  class C(val x: () => Int):
    val y: {*} C = this

  def f = () =>
    eff
    1

  def c1 = new C(f)
  def c2 = c1
  def c3 = c2.y
  val _ = c3: {*} C
