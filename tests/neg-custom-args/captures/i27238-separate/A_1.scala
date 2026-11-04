import language.experimental.captureChecking
import language.experimental.separationChecking
import scala.caps.{any, Control, Mutable}

class Ref extends Mutable:
  var x = 0
  update def set(v: Int): Unit = x = v

def fork(consume t: => Unit): Unit = ???
def forkExcept(consume t: ->{any.except[Control]} Unit): Unit = ???
