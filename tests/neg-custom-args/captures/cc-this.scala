@annotation.capability class Cap

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


