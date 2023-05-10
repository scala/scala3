import language.experimental.captureChecking

class Unit
object u extends Unit

type Foo[X] = [T] -> (op: X => T) -> T
type Lazy[X] = Unit => X

def force[X](fx: Foo[Lazy[X]]): X =
  fx[X](f => f(u))

def force2[X](fx: Foo[Unit => X]): X =
  fx[X](f => f(u))
