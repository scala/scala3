import caps.cap
import caps.unbox

class Unit
object u extends Unit

type Top = Any^

type Wrapper[+T] = [X] -> (op: T ->{cap} X) -> X

def test =

  def wrapper[T](x: T): Wrapper[T] =
    [X] => (op: T ->{cap} X) => op(x)

  def strictMap[A <: Top,  B <: Top](mx: Wrapper[A])(f: A ->{cap} B): Wrapper[B] =
    mx((x: A) => wrapper(f(x)))

  def force[A](thunk: Unit ->{cap} A): A = thunk(u)

  def forceWrapper[A](@unbox mx: Wrapper[Unit ->{cap} A]): Wrapper[A] =
    // Γ ⊢ mx: Wrapper[□ {cap} Unit => A]
    // `force` should be typed as ∀(□ {cap} Unit -> A) A, but it can not
    strictMap[Unit ->{mx*} A, A](mx)(t => force[A](t)) // error // should work
