//> using options -source 3.5
// (to make sure we use the unsealed policy)
class Unit
object unit extends Unit

type Top = Any^

type LazyVal[T] = Unit => T

class Foo[T](val x: T)

// Foo[□ Unit => T]
type BoxedLazyVal[T] = Foo[LazyVal[T]]

def force[A](v: BoxedLazyVal[A]): A =
  // Γ ⊢ v.x : □ {any} Unit -> A
  v.x(unit)  // error: (unbox v.x)(unit), was ok under the sealed policy