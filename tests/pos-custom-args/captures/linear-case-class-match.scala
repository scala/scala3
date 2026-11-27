import language.experimental.separationChecking
import caps.*

class Ref extends Mutable:
  var x = 0
  def get: Int = x
  update def put(y: Int): Unit = x = y

trait FreshList[+A] { this: FreshList[A]^ =>
  def isEmpty: Boolean
}

case class FreshCons[+A](consume val head: A^, val tail: FreshList[A]^) extends FreshList[A]:
  def isEmpty = false

object FreshNil extends FreshList[Nothing]:
  def isEmpty = true

def par[A, B](x: A^, y: B^): Unit = ()

// Disjoint sum-type cases
def disjointCases(consume xs: FreshList[Ref^{}]^): Unit = xs match
  case FreshCons(h, t) => h.put(1)
  case FreshNil => ()

// Multiple linear case-class patterns with structural subpatterns
def nestedSubpatternFallthrough(consume xs: FreshList[Ref^{}]^): Unit = xs match
  case FreshCons(h, FreshNil) => h.put(1)
  case FreshCons(h, FreshCons(h2, t)) => par(h, h2)
  case FreshCons(h, t) => h.put(2)
  case FreshNil => ()

// Fallback accessing the scrutinee when earlier structural linear case fails
def fallbackUsesScrutinee(consume xs: FreshList[Ref^{}]^): Boolean = xs match
  case FreshCons(h, t) => false
  case _ => xs.isEmpty

// Conditional logic inside the case body rather than a pattern guard
def guardInsideBody(consume xs: FreshList[Ref^{}]^): Unit = xs match
  case FreshCons(h, t) =>
    if h.get == 0 then h.put(1)
    else h.put(2)
  case FreshNil => ()

// Structural alternatives on a linear case class
def alternativePatterns(consume xs: FreshList[Ref^{}]^): Boolean = xs match
  case FreshCons(_, FreshNil) | FreshCons(_, FreshCons(_, FreshNil)) => true
  case _ => xs.isEmpty

// Linear enum case class
enum LinList[+A]:
  case Cons(consume head: A^, consume tail: LinList[A]^)
  case Nil

def matchLinearEnum(consume xs: LinList[Ref^{}]^): Unit = xs match
  case LinList.Cons(h, LinList.Nil) => h.put(1)
  case LinList.Cons(h, LinList.Cons(h2, _)) => par(h, h2)
  case LinList.Cons(h, t) => h.put(2)
  case LinList.Nil => ()

// Local linear case class
def localLinearCaseClass(consume r: Ref^): Unit =
  trait Wrap { this: Wrap^ => def isEmpty: Boolean }
  case class Full(consume val ref: Ref^) extends Wrap { def isEmpty = false }
  object Empty extends Wrap { def isEmpty = true }
  val w: Wrap^ = Full(r)
  w match
    case Full(x) => x.put(1)
    case _ =>
      val _ = w.isEmpty
