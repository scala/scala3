class Unit
object unit extends Unit

type Top = {*} Any

type LazyVal[T] = {*} Unit -> T

class Foo[T](val x: T)

// Foo[□ {*} Unit -> T]
type BoxedLazyVal[T] = Foo[LazyVal[T]]

def force[A](v: BoxedLazyVal[A]): A =
  // Γ ⊢ v.x : □ {*} Unit -> A
  v.x(unit)  // error: (unbox v.x)(unit), where (unbox v.x) should be untypable