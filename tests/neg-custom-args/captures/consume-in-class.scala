import language.experimental.separationChecking
import caps.*
class Ref extends Mutable:
  var x = 0
  def get: Int = x
  update def put(y: Int): Unit = x = y
  consume def dealloc(): Unit = ()
case class FreshRef(val a: Ref^):
  a.dealloc()  // error
def boom(consume x: FreshRef^): Unit =
  x match
    case FreshRef(core) => core.dealloc()  // boom! double-free

class FreshRef1(consume val a: Ref^):
  val r: Ref^ = a // error

def foo(a: Ref^) =
  a.dealloc()  // error
  a.dealloc()  // error
