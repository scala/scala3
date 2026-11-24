import language.experimental.captureChecking

trait Cap extends caps.SharedCapability
trait F[+C <: F[C]] extends Cap:
  def self: C

def f(x: Any) = x match
  case s: F[_] => s.self // error
  case _ => null
