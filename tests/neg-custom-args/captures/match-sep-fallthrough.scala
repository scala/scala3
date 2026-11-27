import language.experimental.separationChecking
import caps.*

class Box extends Mutable:
  var x = 0
  def get: Int = x
  update def inc(): Unit = x += 1

def consumeBox(consume b: Box^): Boolean = false

def guardFallthrough(n: Int, consume b: Box^): Unit =
  n match
    case 1 if consumeBox(b) => ()
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

trait FreshList[+A] { this: FreshList[A]^ =>
  def isEmpty: Boolean
}

case class FreshCons[+A](consume val head: A^, val tail: FreshList[A]^) extends FreshList[A]:
  def isEmpty = false

object FreshNil extends FreshList[Nothing]:
  def isEmpty = true

def par[A, B](x: A^, y: B^): Unit = ()

// A guard on a linear case-class pattern can observe or leak extracted components,
// so the scrutinee remains consumed on fallthrough.
def linearGuardFallthrough(consume xs: FreshList[Box^{}]^): Unit =
  xs match
    case FreshCons(h, t) if h.get == 0 => h.inc()
    case FreshCons(h, t) => h.inc() // error
    case FreshNil => ()

// Guard leaking extracted component into a mutable cell cannot fall through and unpack again
def linearGuardLeak(consume xs: FreshList[Box^{}]^): Unit =
  class Cell(var r: Box^) extends Mutable
  val cell = Cell(Box())
  xs match
    case FreshCons(h, t) if { cell.r = h; false } => () // error
    case FreshCons(h2, t2) => par(cell.r, h2) // error
    case FreshNil => ()

// A nested user-defined extractor inside a linear case-class pattern can observe
// extracted components, so the scrutinee remains consumed on fallthrough.
def linearUserSubExtractor(consume xs: FreshList[Box^{}]^): Unit =
  object Peek:
    def unapply(t: FreshList[Box^{}]^): Boolean = false
  xs match
    case FreshCons(h, Peek()) => ()
    case FreshCons(h, t) => h.inc() // error
    case FreshNil => ()

// Unpacking the same scrutinee twice in one pattern is still rejected
def linearDoubleUnpack(consume xs: FreshList[Box^{}]^): Unit =
  (xs, xs) match
    case (FreshCons(h, t), FreshCons(h2, t2)) => par(h, h2) // error
    case _ => ()

// Using the scrutinee inside the matching case body or after the match is rejected
def linearUseInBodyAndAfter(consume xs: FreshList[Box^{}]^): Unit =
  xs match
    case FreshCons(h, t) =>
      val _ = xs.isEmpty // error
      h.inc()
    case FreshNil => ()
  val _ = xs.isEmpty // error

// Binding the scrutinee with an @-pattern alongside a linear extractor
def linearAsPattern(consume xs: FreshList[Box^{}]^): Unit =
  xs match
    case c @ FreshCons(h, t) =>
      val _ = c.isEmpty // error
      h.inc()
    case FreshNil => ()
