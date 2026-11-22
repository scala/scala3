trait F[+C <: F[C]]:
  def self: C

def fWild(x: Any) = x match
  case s: F[_] => s.self
  case _ => null

def fVar(x: Any) = x match
  case s: F[t] => s.self
  case _ => null

trait Bar[+X]
trait Mut[+A <: Bar[B], +B <: Mut[A, B]]

def fMutWild(x: Any) = x match
  case s: Mut[_, _] =>

def fMutVar(x: Any) = x match
  case s: Mut[a, b] =>
