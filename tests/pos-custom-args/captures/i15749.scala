import caps.use
class U
object u extends U

type Top = Any^

type LazyVal[T, C^] = U ->{C} T

class Foo[T](val x: T)

// Foo[□ U => T]
type BoxedLazyVal[T, C^] = Foo[LazyVal[T, C]]
/*
def force[A, C^](v: BoxedLazyVal[A, C]): A =
  // Γ ⊢ v.x : □ {any} U -> A
  v.x(u)  // should be error: (unbox v.x)(u), where (unbox v.x) should be untypable, now ok
*/
def force[A, C^](v: Foo[U ->{C} A]): A =
  // Γ ⊢ v.x : □ {any} U -> A
  v.x(u)  // should be error: (unbox v.x)(u), where (unbox v.x) should be untypable, now ok