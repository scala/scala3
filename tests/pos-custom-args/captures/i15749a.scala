import caps.cap
import caps.use


class Unit
object u extends Unit

type Top = Any^

class Wrapper[+T](val value: [X] -> (op: T ->{cap} X) -> X)

def test =

  def wrapper[T](x: T): Wrapper[T] = Wrapper:
    [X] => (op: T ->{cap} X) => op(x)

  def strictMap[A <: Top, B <: Top, C^](mx: Wrapper[A])(f: A ->{cap, C} B): Wrapper[B] =
    mx.value((x: A) => wrapper(f(x)))

  def force[A](thunk: Unit ->{cap} A): A = thunk(u)

  def forceWrapper[A, C^](mx: Wrapper[Unit ->{C} A]): Wrapper[A] =
    strictMap[Unit ->{C} A, A, C](mx)(t => force[A](t)) // was error when Wrapper was an alias type
