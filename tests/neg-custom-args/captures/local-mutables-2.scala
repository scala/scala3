import caps.*
class Ref extends Mutable:
  var cur = 0
  def get: Int = cur
  update def set(x: Int): Unit = cur = x

def foo() = // error: separation failure
  val r = Ref()
  (() => r.get, (x: Int) => r.set(x))

def foo2() = // error: separation failure
  var r: Int = 0
  (() => r, (x: Int) => r = x)

class Struct:
  private val r: Ref^ = Ref()
  val getter = () => r.get
  val setter = (x: Int) => r.set(x)

def bar() = Struct()  // ok
