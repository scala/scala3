import caps.cap
import caps.use

class Unit
object u extends Unit

type Top = Any^

class Wrapper[+T](val value: [X] -> (op: T ->{cap} X) -> X)

def test =

  def wrapper[T](x: T): Wrapper[T] = Wrapper:
    [X] => (op: T ->{cap} X) => op(x)

  def strictMap[A <: Top,  B <: Top](mx: Wrapper[A])(f: A ->{cap} B): Wrapper[B] =
    mx.value((x: A) => wrapper(f(x)))

  def force[A](thunk: Unit ->{cap} A): A = thunk(u)

  def forceWrapper[A](@use mx: Wrapper[Unit ->{cap} A]): Wrapper[A] =
    strictMap[Unit ->{mx*} A, A](mx)(t => force[A](t)) // was error when Wrapper was an alias type
