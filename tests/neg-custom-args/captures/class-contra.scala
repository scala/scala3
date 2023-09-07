
class C
type Cap = C^

class K(val f: T^):
  def setf(x: T^{f}) = ???

class T

def test(x: Cap, y: Cap) =
  val a: T^{x, y} = ???
  def fun(x: K{val f: T^{a}}) = x.setf(a) // error
  ()