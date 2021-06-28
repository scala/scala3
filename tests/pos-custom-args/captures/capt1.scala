class C
type Cap = C retains *
type Top = Any retains *
def f1(c: Cap): (() => c.type) retains c.type = () => c // ok

def f2: Int =
  val g: (Boolean => Int) retains * = ???
  val x = g(true)
  x

def f3: Int =
  def g: (Boolean => Int) retains * = ???
  def h = g
  val x = g.apply(true)
  x

def foo() =
  val x: C retains * = ???
  val y: C retains x.type = x
  val x2: (() => C) retains x.type = ???
  val y2: (() => C retains x.type) retains x.type = x2

  val z1: (() => Cap) retains * = f1(x)
  def h[X <:Top](a: X)(b: X) = a

  val z2 =
    if x == null then () => x else () => C()
