import language.experimental.captureChecking
import language.experimental.separationChecking
import scala.caps.{any, Control, Mutable}

class Ref extends Mutable:
  var x = 0
  update def set(v: Int): Unit = x = v

def fork(consume t: ->{any.except[Control]} Unit): Unit = ???

// What the argument of a consume by-name parameter captures is consumed
def test(consume r: Ref^) =
  fork(r.set(1))      // ok
  r.set(2)            // error
