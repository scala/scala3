import annotation.retains
class B
type Cap = B{ref any}
class C(val n: Cap):
  this: C{ref n} =>
  def foo(): B{ref n} = n


def test(x: Cap, y: Cap, z: Cap) =
  val c0 = C(x)
  val c1: C{ref x}{val n: B{ref x}} = c0
  val d = c1.foo()
  d: B{ref x}

  val c2 = if ??? then C(x) else C(y)
  val c2a = identity(c2)
  val c3: C{ref x, y}{ val n: B{ref x, y} } = c2
  val d1 = c3.foo()
  d1: B{ref x, y}

  class Local:

    def this(a: Cap) =
      this()
      if a == z then println("?")

    val f = y
    def foo = x
  end Local

  val l = Local()
  val l1: Local{ref x, y} = l
  val l2 = Local(x)
  val l3: Local{ref x, y, z} = l2

