import annotation.capability

@capability class Cap
def f1(c: Cap): {c} () -> c.type = () => c // ok

def f2: Int =
  val g: Boolean => Int = ???
  val x = g(true)
  x

def f3: Int =
  def g: Boolean => Int = ???
  def h = g
  val x = g.apply(true)
  x

def foo() =
  val x: Cap = ???
  val y: Cap = x
  val x2: {x} () -> Cap = ???
  val y2: {x} () -> Cap = x2

  val z1: () => Cap = f1(x)
  def h[X](a: X)(b: X) = a

  val z2 =
    if x == null then () => x else () => Cap()
  x
