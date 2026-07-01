// Test case for TypeParamRef handling in jsig
// jsig matches: case ref @ TypeParamRef(_: PolyType, _)

class TypeParamRefTest:
  // Simple type parameter
  def identity[A](x: A): A = x

  // Type parameter with upper bound
  def bounded[T <: String](x: T): T = x

  // Multiple type parameters
  def pair[A, B](a: A, b: B): (A, B) = (a, b)

  // Type parameter with context bound
  def withOrdering[T: Ordering](x: T, y: T): T =
    if summon[Ordering[T]].lt(x, y) then x else y

  // Type parameter with lower bound
  def covariant[T >: String](x: T): T = x

  // Type parameter in nested position
  def mapList[A, B](lst: List[A], f: A => B): List[B] =
    lst.map(f)

  // Polymorphic method with return type as type param
  def getFirst[T](xs: List[T]): Option[T] =
    xs.headOption

  // Multiple constraints on type parameter
  def withConstraints[T](x: T): Unit =
    ()

  // Type parameter used in method type
  def methodWithPolyParam[A](f: (A => A)): Unit =
    ()

  // Class with type parameters
  class Container[T](val value: T):
    def getValue: T = value
    def map[U](f: T => U): U = f(value)
