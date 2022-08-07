class Unit
object unit extends Unit

type Top = {*} Any

type Wrapper[T] = [X] -> (op: {*} T -> X) -> X

def test =

  def wrapper[T](x: T): Wrapper[T] =
    [X] => (op: {*} T -> X) => op(x)

  def strictMap[A <: Top, B <: Top](mx: Wrapper[A])(f: {*} A -> B): Wrapper[B] =
    mx((x: A) => wrapper(f(x)))

  def force[A](thunk: {*} Unit -> A): A = thunk(unit)

  def forceWrapper[A](mx: Wrapper[{*} Unit -> A]): Wrapper[A] =
    // Γ ⊢ mx: Wrapper[□ {*} Unit => A]
    // `force` should be typed as ∀(□ {*} Unit -> A) A, but it can not
    strictMap[{*} Unit -> A, A](mx)(t => force[A](t)) // error
