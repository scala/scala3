import annotation.retains
class B
type Cap = B^
class C(val n: Cap):
  this: C^{n} =>
  def foo(): B^{n} = n


def test(x: Cap, y: Cap, z: Cap) =
  val c0 = C(x)
  val c1: C{val n: B^{x}}^{x} = c0
  val d = c1.foo()
  d: B^{x}

  val c2 = if ??? then C(x) else C(y)
  val c2a = identity(c2)
  val c3: C{ val n: B^{x, y} }^{x, y} = c2
  val d1 = c3.foo()
  d1: B^{x, y}

  class Local:

    def this(a: Cap) =
      this()
      if a == z then println("?")

    val f = y
    def foo = x
  end Local

  val l = Local()
  val l1: Local^{x, y} = l
  val l2 = Local(x)
  val l3: Local^{x, y, z} = l2

