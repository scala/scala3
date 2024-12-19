def test2(x1: (() => Unit), x2: (() => Unit) => Unit) =
  class C1(x1: (() => Unit), xx2: (() => Unit) => Unit):
    def c2(y1: (() => Unit), y2: (() => Unit) => Unit): C2^ = ???
    class C2(y1: (() => Unit), y2: (() => Unit) => Unit)

  def test3(y1: (() => Unit), y2: (() => Unit) => Unit) =
    val cc1: C1^{y1, y2} = C1(y1, y2)
    val cc2 = cc1.c2(x1, x2)
    val cc3: cc1.C2^{cc1, x1, x2} = cc2

