import caps.Capability

def f1(c: Capability): () ->{c} c.type = () => c // ok

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
  val x: Capability = ???
  val y: Capability = x
  val x2: () ->{x} Capability = ???
  val y2: () ->{x} Capability = x2

  val z1: () => Capability = f1(x)
  def h[X](a: X)(b: X) = a

  val z2 =
    if x == null then () => x else () => new Capability() {}
  val _ = x

