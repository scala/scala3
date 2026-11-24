import language.experimental.separationChecking
import caps.*

class Box extends Mutable:
  var x = 0
  update def inc(): Unit = x += 1

def consumeBox(consume b: Box^): Boolean = false

def guardFallthrough(n: Int, consume b: Box^): Unit =
  n match
    case 1 if consumeBox(b) => ()
    case _ => b.inc() // error

object ConsumeEx:
  def unapply(consume b: Box^): Option[Int] = None

def extractorFallthrough(consume b: Box^): Unit =
  b match
    case ConsumeEx(_) => ()
    case _ => b.inc() // error

def tryGuardFallthrough(consume b: Box^): Unit =
  try ()
  catch
    case _ if consumeBox(b) => ()
    case _ => b.inc() // error

// Consuming only in a case body does not affect subsequent cases
def bodyNoFallthrough(n: Int, consume b: Box^): Unit =
  n match
    case 1 => consumeBox(b)
    case _ => b.inc()
