
def f(x: (() => Unit)): (() => Unit) => (() => Unit) =
  def g(y: (() => Unit)): (() => Unit) = x
  g

def test1(x: (() => Unit)): Unit =
  def test2(y: (() => Unit)) =
    val a: (() => Unit) => (() => Unit) = f(y)
    a(x)            // OK, but should be error
  test2(() => ())

def test2(x1: (() => Unit), x2: (() => Unit) => Unit) =
  class C1(x1: (() => Unit), xx2: (() => Unit) => Unit):
    def c2(y1: (() => Unit), y2: (() => Unit) => Unit): C2^ = C2(y1, y2)
    class C2(y1: (() => Unit), y2: (() => Unit) => Unit):
      val a: (() => Unit) => (() => Unit) = f(y1)
      a(x1)            //OK, but should be error
    C2(() => (), x => ())

  def test3(y1: (() => Unit), y2: (() => Unit) => Unit) =
    val cc1: C1^{y1, y2} = C1(y1, y2)
    val cc2 = cc1.c2(x1, x2)
    val cc3: cc1.C2^{cc2} = cc2

