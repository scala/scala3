class A
class B extends A

class Map[T] { def foo(x: T): A = new A }

class AnyRefMap[T <: AnyRef] extends Map[T] {
  // This is an overload in Scala 2 but an override in Dotty
  def foo(y: T with AnyRef): B = new B // error: missing override modifier
}
