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
    def c2(y1: (() => Unit), y2: (() => Unit) => Unit): C2^{cap} = C2(y1, y2)
    class C2(y1: (() => Unit), y2: (() => Unit) => Unit):
      val a: (() => Unit) => (() => Unit) = f(y1)
      a(x1)            //OK, but should be error
    C2(() => (), x => ())

  def test3(y1: (() => Unit), y2: (() => Unit) => Unit) =
    val cc1 = C1(y1, y2)
    val cc2 = cc1.c2(x1, x2)
    val cc3: cc1.C2^{cap[test2]} = cc2 // error

def test4(x1: () => Unit) =
  class C1:
    this: C1^ =>
    class C2(z: () => Unit):
      this: C2^ =>
      val foo: () => Unit = ???

  def test5(x2: () => Unit) =
    val xx1: C1^{cap[test5]} = C1()
    val y1 =
      val xx2 = xx1.C2(x1)
      val xx3: xx1.C2^{cap[test4]} = xx2 // ok, but dubious
                                       // actual capture set is in test4
                                       // but level constraints would determine that the root should be in test5
                                       // only, there is no root in the set to be mapped
      xx2
    val f1 = y1.foo
    val xx4 = xx1.C2(x2)
    val xx5: xx1.C2^{cap[test4]} = xx4  // error

