
class C
type Cap = {*} C

class K(val f: {*} T):
  def setf(x: {f} T) = ???

class T

def test(x: Cap, y: Cap) =
  val a: {x, y} T = ???
  def fun(x: K{val f: {a} T}) = x.setf(a) // error
  ()