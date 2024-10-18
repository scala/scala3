
class C
type Cap = C^

class K(val f: T^):
  def setf(x: T^{f}) = ???

class T

def test(x: Cap, y: Cap) =
  val a: T^{x, y} = ???
  def fun1(k: K{val f: T^{a}}) = k.setf(a) // error
  def fun2(k: K{val f: a.type}) = k.setf(a)
  ()