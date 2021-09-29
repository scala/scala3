class C
type Cap = {*} C
def f1(c: Cap): {c} () => c.type = () => c // ok

def f2: Int =
  val g: {*} Boolean => Int = ???
  val x = g(true)
  x

def f3: Int =
  def g: {*} Boolean => Int = ???
  def h = g
  val x = g.apply(true)
  x

def foo() =
  val x: {*} C = ???
  val y: {x} C = x
  val x2: {x} () => C = ???
  val y2: {x} () => {x} C = x2

  val z1: {*} () => Cap = f1(x)
  def h[X](a: X)(b: X) = a

  val z2 =
    if x == null then () => x else () => C()
  x