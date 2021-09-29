class B
type Cap = {*} B
class C(val n: Cap):
  this: ({n} C) =>
  def foo(): {n} B = n


def test(x: Cap, y: Cap, z: Cap) =
  val c0 = C(x)
  val c1: {x} C {val n: {x} B} = c0
  val d = c1.foo()
  d: ({x} B)

  val c2 = if ??? then C(x) else C(y)
  val c2a = identity(c2)
  val c3: {x, y} C { val n: {x, y} B } = c2
  val d1 = c3.foo()
  d1: B @retains(x, y)

  class Local:

    def this(a: Cap) =
      this()
      if a == z then println("?")

    val f = y
    def foo = x
  end Local

  val l = Local()
  val l1: {x, y} Local = l
  val l2 = Local(x)
  val l3: {x, y, z} Local = l2

