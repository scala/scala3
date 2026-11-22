import language.experimental.captureChecking

implicit class EnrichedIterable[
    A,
    Iter[X] <: scala.collection.IterableOps[X, Iter, Iter[X]]
](private val iterable: Iter[A])
    extends AnyVal {

  def bad = iterable match {
    case set: scala.collection.SetOps[_, _, _] =>
  }
}

trait F[+C <: F[C]]:
  def self: C

def fWild(x: Any) = x match
  case s: F[_] => s.self
  case _ => null

def fVar(x: Any) = x match
  case s: F[t] => s.self
  case _ => null

trait Inv[C <: Inv[C]]
def fInv(x: Any) = x match
  case s: Inv[_] =>

trait Bar[+X]
trait Mut[+A <: Bar[B], +B <: Mut[A, B]]
def fMutWild(x: Any) = x match
  case s: Mut[_, _] =>
def fMutVar(x: Any) = x match
  case s: Mut[a, b] =>

trait Cap extends caps.SharedCapability
trait CapF[+C <: CapF[C]] extends Cap
def fCap(x: Any) = x match
  case s: CapF[_] =>
