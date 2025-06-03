import language.experimental.captureChecking
import annotation.unchecked.uncheckedCaptures

class Unit
object u extends Unit

type Foo[X] = [T] -> (op: X => T) -> T
type Lazy[X] = Unit => X

def force[X](fx: Foo[Lazy[X] @uncheckedCaptures]): X =
  fx[X](f => f(u))

def force2[X](fx: Foo[(Unit => X) @uncheckedCaptures]): X =
  fx[X](f => f(u))
