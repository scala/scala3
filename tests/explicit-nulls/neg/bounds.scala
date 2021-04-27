// Test Null type in bounds

class Test {
  val x1: String = ???
  val x2: String | Null = ???

  // T has to be nullable type
  def f1[T >: Null <: AnyRef | Null](x: T): T = x

  // Null is no longer a subtype of AnyRef, so it is impossible to apply this method directly.
  // However, defining this kind of functions is allowed.
  // We can bypass this restriction by importing unsafeNulls.
  def f2[T >: Null <: AnyRef](x: T): T = x

  def nullOf[T >: Null <: AnyRef | Null]: T = null

  def g = {
    f1(x1)
    f1(x2)

    f2(x1) // error
    f2(x2) // error

    val n1: String = nullOf // error
    val n3: String | Null = nullOf
  }

  // Bounds in class definition is strictly checked
  class A[T >: Null <: AnyRef] {} // error: conflicting bounds Null <: ... <: AnyRef
}
